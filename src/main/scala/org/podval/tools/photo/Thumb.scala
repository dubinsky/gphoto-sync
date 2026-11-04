package org.podval.tools.photo

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Metadata
import com.drew.metadata.exif.{ExifSubIFDDirectory, ExifThumbnailDirectory}
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, IOException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, Semaphore, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import javax.imageio.stream.ImageInputStream
import scala.annotation.tailrec
import scala.util.control.NonFatal

// One pipeline for a stem. thm and tif are never sources. A miss is not stored:
// a later ffmpeg install must not keep serving a cached failure.
object Thumb:
  private[photo] enum Display:
    case Inline(bytes: Array[Byte], contentType: String)
    case Stream(file: File, contentType: String)

  private val thumbEdge: Int = 256
  private val fullEdge: Int = 1600
  private val thumbCap: Int = 8 * 1024 * 1024
  private val fullCap: Int = 20 * 1024 * 1024
  private val jpegQuality: Float = 0.85f

  private val stills: Seq[String] = Seq(Extension.JPG.name, Extension.JPEG.name, Extension.GIF.name)
  private val embeddedStills: Seq[String] = Seq(
    Extension.WEBP.name,
    Extension.HEIC.name,
    Extension.HEIF.name,
    Extension.AVIF.name
  )
  private val raws: Seq[String] = Seq(
    Extension.CRW.name,
    Extension.CR2.name,
    Extension.CR3.name,
    Extension.NEF.name,
    Extension.ORF.name,
    Extension.ARW.name,
    Extension.RW2.name,
    Extension.RWL.name,
    Extension.SRW.name
  )
  private val videos: Seq[String] = Seq(
    Extension.MOV.name,
    Extension.MP4.name,
    Extension.M4V.name,
    Extension.AVI.name,
    Extension.TGP.name
  )

  // jpg, jpeg, gif, then png, webp, heic, heif, avif, then raw, then video.
  private[photo] val sourceOrder: Seq[String] =
    stills ++ Seq(Extension.PNG.name) ++ embeddedStills ++ raws ++ videos

  private val stillSet: Set[String] = stills.toSet
  private val embeddedSet: Set[String] = embeddedStills.toSet
  private val rawSet: Set[String] = raws.toSet
  private val videoSet: Set[String] = videos.toSet
  private val jpegSet: Set[String] = Set(Extension.JPG.name, Extension.JPEG.name)

  private val decoders = new Semaphore(4)
  private val ffmpegSlots = new Semaphore(2)
  private val inflight = new ConcurrentHashMap[String, CompletableFuture[Option[Array[Byte]]]]()
  private val ffmpegLogged = new AtomicBoolean(false)
  private val ffmpegMissing = new AtomicBoolean(false)
  private val encodeLogged = new AtomicBoolean(false)
  private val dirMode = PosixFilePermissions.fromString("rwx------")
  private val fileMode = PosixFilePermissions.fromString("rw-------")

  private enum Shot:
    case Frame(bytes: Array[Byte])
    case Again
    case Stop

  private[photo] lazy val placeholder: Array[Byte] =
    val path: String = "org/podval/tools/photo/web/placeholder.jpg"
    val in = getClass.getClassLoader.getResourceAsStream(path)
    if in == null then throw IllegalStateException("missing placeholder")
    else
      try in.readAllBytes()
      finally in.close()

  private[photo] def thumbnail(relative: String, files: Seq[(String, File)]): Option[Array[Byte]] =
    first(ordered(files)): (ext, file) =>
      val material: String = s"$relative\n$ext\n${file.lastModified}\n${file.length}"
      load(material):
        thumbBytes(ext, file)

  private[photo] def display(files: Seq[(String, File)]): Option[Display] =
    first(ordered(files)): (ext, file) =>
      displayOne(ext, file)

  // metadata-extractor 2.22.0. A throw, or a slice that is not a JPEG, is a miss.
  private[photo] def embeddedJpeg(file: File, maxLength: Int): Option[Array[Byte]] =
    try
      val metadata: Metadata = ImageMetadataReader.readMetadata(file)
      thumbSlice(file, metadata, maxLength).orElse(subIfdSlice(file, metadata, maxLength))
    catch case NonFatal(_) => None

  private def thumbSlice(file: File, metadata: Metadata, maxLength: Int): Option[Array[Byte]] =
    Option(metadata.getFirstDirectoryOfType(classOf[ExifThumbnailDirectory])).flatMap: directory =>
      val offset: Option[Long] = Option(directory.getAdjustedThumbnailOffset).map(_.toLong)
      val length: Option[Int] = Option(directory.getInteger(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH))
      offset.flatMap(start => length.flatMap(size => readSlice(file, start, size, maxLength)))

  // NEF JpgFromRaw stays on ExifSubIFDDirectory as unknown tags 0x0201 and 0x0202.
  private def subIfdSlice(file: File, metadata: Metadata, maxLength: Int): Option[Array[Byte]] =
    Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory])).flatMap: directory =>
      val offset: Option[Long] = Option(directory.getInteger(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET)).map(_.toLong)
      val length: Option[Int] = Option(directory.getInteger(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH))
      offset.flatMap(start => length.flatMap(size => readSlice(file, start, size, maxLength)))

  private def readSlice(file: File, offset: Long, length: Int, maxLength: Int): Option[Array[Byte]] =
    if offset < 0 || length <= 0 || length > maxLength || offset > file.length - length then None
    else
      val bytes = new Array[Byte](length)
      val input = new RandomAccessFile(file, "r")
      try
        input.seek(offset)
        input.readFully(bytes)
        if isJpeg(bytes) then Some(bytes) else None
      catch case NonFatal(_) => None
      finally input.close()

  private def ordered(files: Seq[(String, File)]): Seq[(String, File)] =
    val byExt: Map[String, File] = files.toMap
    sourceOrder.flatMap(ext => byExt.get(ext).map(ext -> _))

  @tailrec
  private def first[A](rest: Seq[(String, File)])(body: (String, File) => Option[A]): Option[A] =
    rest match
      case Nil =>
        None
      case (ext, file) +: tail =>
        body(ext, file) match
          case some @ Some(_) => some
          case None => first(tail)(body)

  private def thumbBytes(ext: String, file: File): Option[Array[Byte]] =
    if stillSet.contains(ext) || ext == Extension.PNG.name then
      scaled(thumbEdge, jpegSet.contains(ext)):
        ImageIO.createImageInputStream(file)
    else if embeddedSet.contains(ext) || rawSet.contains(ext) then
      embeddedJpeg(file, thumbCap).flatMap: bytes =>
        scaled(thumbEdge, subsample = true):
          ImageIO.createImageInputStream(new ByteArrayInputStream(bytes))
    else if videoSet.contains(ext) then
      ffmpeg(file, thumbEdge)
    else None

  private def displayOne(ext: String, file: File): Option[Display] =
    if jpegSet.contains(ext) then Some(Display.Stream(file, "image/jpeg"))
    else if ext == Extension.GIF.name then Some(Display.Stream(file, "image/gif"))
    else if ext == Extension.PNG.name then Some(Display.Stream(file, "image/png"))
    else if embeddedSet.contains(ext) || rawSet.contains(ext) then
      embeddedJpeg(file, fullCap).map(bytes => Display.Inline(bytes, "image/jpeg"))
    else if videoSet.contains(ext) then
      ffmpeg(file, fullEdge).map(bytes => Display.Inline(bytes, "image/jpeg"))
    else None

  private def scaled(edge: Int, subsample: Boolean)(open: => ImageInputStream): Option[Array[Byte]] =
    decoders.acquire()
    try
      val stream: ImageInputStream = open
      if stream == null then None
      else
        try decode(stream, edge, subsample).flatMap(encode)
        finally stream.close()
    finally decoders.release()

  private def decode(stream: ImageInputStream, edge: Int, subsample: Boolean): Option[BufferedImage] =
    val readers = ImageIO.getImageReaders(stream)
    if !readers.hasNext then None
    else
      val reader = readers.next()
      try
        reader.setInput(stream, true, true)
        val width: Int = reader.getWidth(0)
        val height: Int = reader.getHeight(0)
        val param = reader.getDefaultReadParam
        if subsample then
          val factor: Int = math.max(1, (math.max(width, height) + edge - 1) / edge)
          if factor > 1 then param.setSourceSubsampling(factor, factor, 0, 0)
        Some(fit(reader.read(0, param), edge))
      catch case _: IOException => None
      finally reader.dispose()

  // Do not upscale. A subsampled JPEG already at or under the edge stays that size.
  private def fit(image: BufferedImage, edge: Int): BufferedImage =
    val width: Int = image.getWidth
    val height: Int = image.getHeight
    val longest: Int = math.max(width, height)
    if longest <= edge then rgb(image, width, height)
    else rgb(image, math.max(1, width * edge / longest), math.max(1, height * edge / longest))

  private def rgb(image: BufferedImage, width: Int, height: Int): BufferedImage =
    val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = out.createGraphics()
    try
      graphics.setColor(Color.WHITE)
      graphics.fillRect(0, 0, width, height)
      graphics.drawImage(image, 0, 0, width, height, null)
      out
    finally graphics.dispose()

  private def encode(image: BufferedImage): Option[Array[Byte]] =
    val writers = ImageIO.getImageWritersByFormatName("jpeg")
    if !writers.hasNext then
      logOnce(encodeLogged, "ImageIO cannot encode jpeg")
      None
    else
      val writer = writers.next()
      val buffer = new ByteArrayOutputStream
      val output = ImageIO.createImageOutputStream(buffer)
      if output == null then
        writer.dispose()
        logOnce(encodeLogged, "ImageIO cannot encode jpeg")
        None
      else
        try
          writer.setOutput(output)
          val param = writer.getDefaultWriteParam
          if param.canWriteCompressed then
            param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
            param.setCompressionQuality(jpegQuality)
          writer.write(null, new IIOImage(image, null, null), param)
          output.flush()
          val bytes: Array[Byte] = buffer.toByteArray
          if isJpeg(bytes) then Some(bytes) else None
        catch case _: IOException =>
          logOnce(encodeLogged, "ImageIO cannot encode jpeg")
          None
        finally
          writer.dispose()
          output.close()

  private def ffmpeg(file: File, edge: Int): Option[Array[Byte]] =
    if ffmpegMissing.get then None
    else
      ffmpegSlots.acquire()
      try
        ffmpegAt(file, edge, "1") match
          case Shot.Frame(bytes) =>
            Some(bytes)
          case Shot.Stop =>
            None
          case Shot.Again if ffmpegMissing.get =>
            None
          case Shot.Again =>
            ffmpegAt(file, edge, "0") match
              case Shot.Frame(bytes) => Some(bytes)
              case _ => None
      finally ffmpegSlots.release()

  // Sequence argv, no shell. ss=1, then ss=0 when that result is empty or not a JPEG.
  // Timeout and a missing binary are a miss and are not retried.
  private def ffmpegAt(file: File, edge: Int, ss: String): Shot =
    if ffmpegMissing.get then Shot.Stop
    else
      val started: Option[Process] =
        try
          Some(new ProcessBuilder(
            "ffmpeg", "-v", "error", "-ss", ss, "-i", file.getAbsolutePath,
            "-frames:v", "1",
            "-vf", s"scale=$edge:$edge:force_original_aspect_ratio=decrease",
            "-f", "image2pipe", "-vcodec", "mjpeg", "pipe:1"
          ).redirectError(ProcessBuilder.Redirect.DISCARD).start())
        catch case error: IOException =>
          // EMFILE and other spawn errors are this request only. A missing binary stays missing.
          if Option(error.getMessage).exists(_.contains("No such file or directory")) then
            ffmpegMissing.set(true)
            logOnce(ffmpegLogged, "ffmpeg is missing")
          None
      started match
        case None =>
          Shot.Stop
        case Some(process) =>
          try process.getOutputStream.close()
          catch case _: IOException => ()
          val output = new ByteArrayOutputStream
          val pump = new Thread(() =>
            try
              val _: Long = process.getInputStream.transferTo(output)
            catch case _: IOException => ()
          , "photo-web-ffmpeg")
          pump.setDaemon(true)
          pump.start()
          try
            val finished: Boolean = process.waitFor(10, TimeUnit.SECONDS)
            if !finished then
              process.destroyForcibly()
              process.waitFor(1, TimeUnit.SECONDS)
            pump.join(1000)
            val bytes: Array[Byte] = output.toByteArray
            if !finished then Shot.Stop
            else if process.exitValue() != 0 then
              if bytes.isEmpty || !isJpeg(bytes) then Shot.Again else Shot.Stop
            else if isJpeg(bytes) then Shot.Frame(bytes)
            else Shot.Again
          finally
            if process.isAlive then process.destroyForcibly()

  private def load(material: String)(generate: => Option[Array[Byte]]): Option[Array[Byte]] =
    val dest: File = cacheFile(material)
    readCached(dest).orElse:
      val created = new CompletableFuture[Option[Array[Byte]]]()
      val raced = inflight.putIfAbsent(material, created)
      if raced != null then raced.join()
      else
        var result: Option[Array[Byte]] = None
        try
          result = readCached(dest).orElse:
            generate.filter(isJpeg).map: bytes =>
              try writeAtomic(dest, bytes)
              catch case _: IOException => ()
              bytes
          result
        catch case NonFatal(_) =>
          result = None
          None
        finally
          created.complete(result)
          inflight.remove(material, created)

  private def cacheRoot: File =
    new File(new File(System.getProperty("user.home"), ".cache/org.podval.tools.photo"), "thumbs")

  private def cacheFile(material: String): File =
    val hex: String = sha256(material)
    new File(new File(cacheRoot, hex.take(2)), hex.drop(2) + ".jpg")

  private def readCached(file: File): Option[Array[Byte]] =
    if !file.isFile then None
    else
      try
        val bytes: Array[Byte] = NFiles.readAllBytes(file.toPath)
        if isJpeg(bytes) then Some(bytes) else None
      catch case _: IOException => None

  private def writeAtomic(dest: File, bytes: Array[Byte]): Unit =
    val temp = new File(dest.getParentFile, s".${dest.getName}.${java.util.UUID.randomUUID}.partial")
    ensureCache(dest)
    val channel: FileChannel = FileChannel.open(
      temp.toPath,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    var moved: Boolean = false
    try
      NFiles.setPosixFilePermissions(temp.toPath, fileMode)
      val buffer: ByteBuffer = ByteBuffer.wrap(bytes)
      while buffer.hasRemaining do channel.write(buffer)
      channel.force(true)
      channel.close()
      NFiles.move(temp.toPath, dest.toPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      moved = true
      NFiles.setPosixFilePermissions(dest.toPath, fileMode)
    finally
      if channel.isOpen then channel.close()
      if !moved && temp.exists() then temp.delete()

  // ~/.cache itself is shared. Only the photo cache directories are 0700.
  private def ensureCache(dest: File): Unit =
    val thumbs: File = cacheRoot
    val photo: File = thumbs.getParentFile
    val shard: File = dest.getParentFile
    if !shard.isDirectory then shard.mkdirs()
    NFiles.setPosixFilePermissions(photo.toPath, dirMode)
    NFiles.setPosixFilePermissions(thumbs.toPath, dirMode)
    NFiles.setPosixFilePermissions(shard.toPath, dirMode)

  private def sha256(text: String): String =
    val digest: Array[Byte] = MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8))
    HexFormat.of().formatHex(digest)

  private def isJpeg(bytes: Array[Byte]): Boolean =
    bytes.length >= 2 && (bytes(0) & 0xff) == 0xff && (bytes(1) & 0xff) == 0xd8

  private def logOnce(flag: AtomicBoolean, line: String): Unit =
    if flag.compareAndSet(false, true) then Console.err.println(line)
