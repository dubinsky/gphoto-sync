package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files as JFiles
import java.time.LocalDate

class AlbumFileTest extends AnyFlatSpec, Matchers:
  "titles" should "round-trip through the start folder" in :
    roundTrip("2010/07/19-08/23", "2010/07/19/to-08-23.toml")
    roundTrip("2010/07/04-18/Cape Cod", "2010/07/04/to-18 Cape Cod.toml")
    roundTrip("2010/07/19", "2010/07/19/album.toml")
    roundTrip("2010/07", "2010/07/album.toml")
    roundTrip("2010/07-08", "2010/07/to-08.toml")
    roundTrip("2010/11-2011/01", "2010/11/to-2011-01.toml")
    roundTrip("2010/12/30-2011/01/02", "2010/12/30/to-2011-01-02.toml")
    roundTrip("2010/07/19-08/23/Name", "2010/07/19/to-08-23 Name.toml")
    roundTrip("2009/03/10/Party", "2009/03/10/Party.toml")
    roundTrip("2005/04/Toronto", "2005/04/Toronto.toml")
    roundTrip("2011/03/07-27", "2011/03/07/to-27.toml")
    roundTrip("Best", "Best.toml")

  it should "canonicalize an equal day and reject an equal month" in :
    val equal: Title.Parsed = Title.parse("2010/07/19-19").getOrElse(fail("equal day"))
    equal.title shouldBe "2010/07/19"
    Title.fileStem(equal) shouldBe "album"
    Title.parse("2010/07-07").isEmpty shouldBe true
    Title.parse("2010/07-2010/07").isEmpty shouldBe true

  it should "reject a hyphen where the end date needs a slash" in :
    Title.parse("2010/07/19-08-23").isEmpty shouldBe true
    Title.parse("2005").isEmpty shouldBe true
    Title.legalName("07") shouldBe false
    Title.legalName("album") shouldBe false
    Title.legalName("to-18") shouldBe false
    Title.legalName("4Name") shouldBe false
    Title.fromLocation("2010/07/19", "to-19").isEmpty shouldBe true
    Title.fromLocation("2010/07", "to-07").isEmpty shouldBe true
    Title.fromLocation("2009", "03").isEmpty shouldBe true

  "include" should "parse bare stems, quoted stems, and ranges" in :
    Include.logical(Include.parse("all except 4217")) shouldBe "all except 4217"
    Include.render(Include.parse("all except 4217")) shouldBe "all except 4217"
    Include.logical(Include.parse("2011/06/12 0664..0843")) shouldBe "2011/06/12 0664..0843"
    val quoted: Include.Entry = Include.parse("2009/03/10 \"4217\"")
    Include.logical(quoted) shouldBe "2009/03/10 4217"
    Include.render(quoted) shouldBe "2009/03/10 4217"
    Include.needsQuote("2009-07-03 13.02.37") shouldBe true
    Include.needsQuote("2010-03-25 14.09.01-1") shouldBe true
    Include.needsQuote("4217") shouldBe false
    val day: LocalDate = LocalDate.of(2011, 6, 12)
    val range: Include.Entry = Include.parse("2011/06/12 0664..0843")
    val span: Option[Title.Span] = Title.parse("2011/06/12").getOrElse(fail("day")).span
    Include.claims(span, range, day, "0700") shouldBe true
    Include.claims(span, range, day, "0663") shouldBe false
    Include.claims(span, range, day.plusDays(1), "0700") shouldBe false
    Include.inside(None, Include.parse("all")) shouldBe false

  it should "read a file that quotes a stem only when a bare token would be wrong" in :
    withOriginals: originals =>
      val text: String =
        "external = true\n" +
        "include = [\n" +
        "  \"2009/07/03 \\\"2009-07-03 13.02.37\\\"..\\\"2009-07-03 13.02.39\\\" except \\\"2009-07-03 13.02.38\\\"\",\n" +
        "  \"2009/07/03 \\\"2010-03-25 14.09.01-1\\\"\",\n" +
        "  \"2009/07/03 \\\"4217\\\"\",\n" +
        "]\n"
      val file: File = write(originals, "2009/07/03/Party.toml", text)
      val album: Album = Album.read(originals, file)
      album.include.map(Include.logical) shouldBe Seq(
        "2009/07/03 2009-07-03 13.02.37..2009-07-03 13.02.39 except 2009-07-03 13.02.38",
        "2009/07/03 2010-03-25 14.09.01-1",
        "2009/07/03 4217"
      )
      album.include.map(Include.render) shouldBe Seq(
        "2009/07/03 \"2009-07-03 13.02.37\"..\"2009-07-03 13.02.39\" except \"2009-07-03 13.02.38\"",
        "2009/07/03 \"2010-03-25 14.09.01-1\"",
        "2009/07/03 4217"
      )

  "the compressor" should "keep a within-day except only when it is shorter" in :
    val day: LocalDate = LocalDate.of(2009, 3, 10)
    val holes: Map[LocalDate, Set[String]] = Map(day -> Set("01", "02", "03", "04", "05", "06"))
    val kept: Set[(LocalDate, String)] = Set("01", "02", "03", "05", "06").map(stem => day -> stem)
    Include.compress(None, kept, holes).map(Include.logical) shouldBe
      Seq("2009/03/10 01..06 except 04")
    val wide: Map[LocalDate, Set[String]] =
      Map(day -> (1 to 10).map(n => f"$n%02d").toSet)
    val ends: Set[(LocalDate, String)] = Set(day -> "01", day -> "10")
    Include.compress(None, ends, wide).map(Include.logical) shouldBe
      Seq("2009/03/10 01", "2009/03/10 10")

  it should "use all except for a Yanovsky-shaped day" in :
    val day: LocalDate = LocalDate.of(2012, 11, 4)
    val low: String = "20121104_200123"
    val high: String = "20121104_205728"
    val wide: String = "20121104_200945_1"
    val universe: Map[LocalDate, Set[String]] = Map(day -> Set(low, high, wide, "6034", "6035", "6036"))
    val claimed: Set[(LocalDate, String)] = Set(day -> low, day -> high, day -> wide)
    val span: Option[Title.Span] = Title.parse("2012/11/04").getOrElse(fail("day")).span
    Include.compress(span, claimed, universe).map(Include.logical) shouldBe
      Seq("all except 6034 6035 6036")
    val (perDay, allExcept) = Include.weights(span, claimed, universe)
    perDay shouldBe 71
    allExcept shouldBe Some(25)

  it should "keep per-day lines when the lengths are equal" in :
    val first: LocalDate = LocalDate.of(2009, 3, 10)
    val second: LocalDate = LocalDate.of(2009, 3, 11)
    val missing: String = "x" * 23
    val universe: Map[LocalDate, Set[String]] = Map(
      first -> Set("01", "02", missing),
      second -> Set("01", "02")
    )
    val claimed: Set[(LocalDate, String)] = Set(
      first -> "01", first -> "02", second -> "01", second -> "02"
    )
    val span: Option[Title.Span] = Title.parse("2009/03/10-11").getOrElse(fail("span")).span
    val lines: Seq[String] = Include.compress(span, claimed, universe).map(Include.logical)
    lines shouldBe Seq("2009/03/10 01..02", "2009/03/11 01..02")
    val (perDay, allExcept) = Include.weights(span, claimed, universe)
    perDay shouldBe 34
    allExcept shouldBe Some(34)
    lines.map(_.length).sum shouldBe perDay

  it should "not merge two days into one range" in :
    val first: LocalDate = LocalDate.of(2009, 3, 10)
    val second: LocalDate = LocalDate.of(2009, 3, 11)
    val universe: Map[LocalDate, Set[String]] = Map(
      first -> Set("01", "02"),
      second -> Set("01", "02")
    )
    val claimed: Set[(LocalDate, String)] = Set(first -> "01", first -> "02", second -> "02")
    val span: Option[Title.Span] = Title.parse("2009/03/10-11").getOrElse(fail("span")).span
    Include.compress(span, claimed, universe).map(Include.logical) shouldBe Seq(
      "2009/03/10 01..02",
      "2009/03/11 02"
    )

  it should "write the compressed lines and read them back" in :
    withOriginals: originals =>
      val day: LocalDate = LocalDate.of(2012, 11, 4)
      val low: String = "20121104_200123"
      val high: String = "20121104_205728"
      val wide: String = "20121104_200945_1"
      val universe: Map[LocalDate, Set[String]] = Map(day -> Set(low, high, wide, "6034", "6035", "6036"))
      val claimed: Set[(LocalDate, String)] = Set(day -> low, day -> high, day -> wide)
      val parsed: Title.Parsed = Title.parse("2012/11/04").getOrElse(fail("day"))
      val include: Seq[Include.Entry] = Include.compress(parsed.span, claimed, universe)
      val album: Album = Album(
        title = parsed.title,
        span = parsed.span,
        include = include,
        external = true,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = "",
        owner = None
      )
      val file: File = Title.file(originals, parsed)
      file.getParentFile.mkdirs()
      Files.write(Album.renderFile(album), file)
      val read: Album = Album.read(originals, file)
      read.include.map(Include.logical) shouldBe Seq("all except 6034 6035 6036")
      val expanded: Set[(LocalDate, String)] = read.include.foldLeft(Set.empty[(LocalDate, String)]):
        (acc, entry) => acc ++ Include.expand(read.span, entry, universe)
      expanded.equals(claimed) shouldBe true

  it should "write an include line as a literal string when it has no apostrophe" in :
    withOriginals: originals =>
      val parsed: Title.Parsed = Title.parse("2009/07/03/Party").getOrElse(fail("day"))
      val album: Album = Album(
        title = parsed.title,
        span = parsed.span,
        include = Seq(
          Include.parse("2009/07/03 \"2009-07-03 13.02.37\""),
          Include.parse("2009/07/03 4217")
        ),
        external = false,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = "",
        owner = None
      )
      val file: File = Title.file(originals, parsed)
      file.getParentFile.mkdirs()
      Files.write(Album.renderFile(album), file)
      val text: String = Files.read(file)
      text shouldBe
        "include = [\n" +
        "  '2009/07/03 \"2009-07-03 13.02.37\"',\n" +
        "  '2009/07/03 4217',\n" +
        "]\n"
      Album.read(originals, file).include.map(Include.logical) shouldBe Seq(
        "2009/07/03 2009-07-03 13.02.37",
        "2009/07/03 4217"
      )

  it should "write a basic string when the include line has an apostrophe" in :
    withOriginals: originals =>
      Include.needsQuote("O'Brien") shouldBe false
      Include.needsQuote("O'Brien day") shouldBe true
      val parsed: Title.Parsed = Title.parse("2009/03/10/Party").getOrElse(fail("day"))
      val album: Album = Album(
        title = parsed.title,
        span = parsed.span,
        include = Seq(
          Include.parse("2009/03/10 O'Brien"),
          Include.parse("2009/03/10 \"O'Brien day\"")
        ),
        external = false,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = "",
        owner = None
      )
      val file: File = Title.file(originals, parsed)
      file.getParentFile.mkdirs()
      Files.write(Album.renderFile(album), file)
      val text: String = Files.read(file)
      text shouldBe
        "include = [\n" +
        "  \"2009/03/10 O'Brien\",\n" +
        "  \"2009/03/10 \\\"O'Brien day\\\"\",\n" +
        "]\n"
      text should not include ("'2009/03/10 O'Brien'")
      Album.read(originals, file).include.map(Include.logical) shouldBe Seq(
        "2009/03/10 O'Brien",
        "2009/03/10 O'Brien day"
      )

  it should "keep a comment and key order through include and google writes" in :
    withOriginals: originals =>
      val original: String =
        "# curated\n" +
        "published-title = \"The Best\"\n" +
        "google-album-id = \"abc\"\n" +
        "include = [\n" +
        "  '2009/03/10 2412',\n" +
        "]\n" +
        "external = true\n"
      val file: File = write(originals, "Best.toml", original)
      Album.rewriteInclude(originals, "2009/03/10/2412", "2009/03/11/2412")
      Album.storeGoogle(file, "abc2", "Renamed")
      val text: String = Files.read(file)
      text should include ("# curated")
      text.indexOf("published-title") should be < text.indexOf("google-album-id")
      text.indexOf("google-album-id") should be < text.indexOf("include")
      text.indexOf("include") should be < text.indexOf("external")
      text should include ("'2009/03/11 2412'")
      text should not include ("2009/03/10")
      text.linesIterator.count(line => line.trim.startsWith("google-album-id")) shouldBe 1

  it should "replace one google-album-id line without adding a second key" in :
    withOriginals: originals =>
      def once(body: String): Unit =
        val file: File = write(originals, "Best.toml", body)
        Album.storeGoogle(file, "next", "Title")
        val text: String = Files.read(file)
        text.linesIterator.count(line => line.trim.startsWith("google-album-id")) shouldBe 1
        text should include ("google-album-id = \"next\"")
        text should not include ("google-album-id=\"id\"")
        text should not include ("google-album-id = \"id\"")

      once(
        "# keep\n" +
        "  google-album-id = \"id\"\n" +
        "include = [\n" +
        "  '2009/03/10 4217',\n" +
        "]\n"
      )
      once("google-album-id=\"id\"\ninclude = ['2009/03/10 4217']\n")
      once("google-album-id\t=\t\"id\"\ninclude = ['2009/03/10 4217']\n")
      once("\"google-album-id\" = \"id\"\ninclude = ['2009/03/10 4217']\n")
      once("'google-album-id' = 'id'\ninclude = ['2009/03/10 4217']\n")

  it should "reject a tab, CR, or LF in a google id and leave the file" in :
    withOriginals: originals =>
      val body: String = "google-album-id = \"id\"\npublished-title = \"T\"\n"
      val file: File = write(originals, "Best.toml", body)
      def reject(id: String, title: String, needle: String): Unit =
        val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
          Album.storeGoogle(file, id, title)
        caught.getMessage should include (needle)
        Files.read(file) shouldBe body

      reject("a\nb", "T", "google-album-id")
      reject("id", "a\nb", "published-title")
      reject("a\tb", "T", "google-album-id")
      reject("a\rb", "T", "google-album-id")

  it should "not write when a google id is not one single line" in :
    withOriginals: originals =>
      val body: String =
        "google-album-id = \"\"\"\n" +
        "id\n" +
        "\"\"\"\n" +
        "include = ['2009/03/10 4217']\n"
      val file: File = write(originals, "Best.toml", body)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.storeGoogle(file, "next", "Title")
      caught.getMessage should include ("google-album-id is not a single line")
      Files.read(file) shouldBe body

  it should "not write when the include array does not end on its own line" in :
    withOriginals: originals =>
      val body: String = "include = [\"2009/03/10 2412\",\n\"2009/01/01 0000\"]\n"
      val file: File = write(originals, "Best.toml", body)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.storeRemovals(file, originals, Seq(LocalDate.of(2009, 3, 10) -> "2412"), Map.empty)
      caught.getMessage should include ("include array does not end on its own line")
      Files.read(file) shouldBe body

  it should "not append a second include when a quoted or tabbed key is not one span" in :
    withOriginals: originals =>
      def reject(body: String): Unit =
        val file: File = write(originals, "Best.toml", body)
        val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
          Album.storeRemovals(file, originals, Seq(LocalDate.of(2009, 3, 10) -> "2412"), Map.empty)
        caught.getMessage should include ("include array does not end on its own line")
        Files.read(file) shouldBe body

      reject("\"include\" = [\"2009/03/10 2412\",\n\"2009/01/01 0000\"]\n")
      reject("'include' = ['2009/03/10 2412',\n'2009/01/01 0000']\n")
      reject("include\t=\t[\"2009/03/10 2412\",\n\"2009/01/01 0000\"]\n")

  it should "rewrite one include line when the key is quoted or separated by tabs" in :
    withOriginals: originals =>
      def once(body: String): Unit =
        val file: File = write(originals, "Best.toml", body)
        Album.storeRemovals(file, originals, Seq(LocalDate.of(2009, 3, 10) -> "2412"), Map.empty)
        val text: String = Files.read(file)
        text shouldBe "include = []\n"
        Album.read(originals, file).include.isEmpty shouldBe true

      once("\"include\" = [\"2009/03/10 2412\"]\n")
      once("'include' = ['2009/03/10 2412']\n")
      once("include\t=\t[\"2009/03/10 2412\"]\n")

  it should "not write when a comment bracket would close the include array" in :
    withOriginals: originals =>
      def reject(body: String): Unit =
        val file: File = write(originals, "Best.toml", body)
        Album.read(originals, file).include.map(Include.logical) shouldBe Seq("2009/03/10 2412")
        val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
          Album.storeRemovals(file, originals, Seq(LocalDate.of(2009, 3, 10) -> "2412"), Map.empty)
        caught.getMessage should include ("include array does not end on its own line")
        Files.read(file) shouldBe body

      reject("include = [\"2009/03/10 2412\", # every picture]\n]\n")
      reject("include = [ # see section 3]\n  \"2009/03/10 2412\",\n]\n")

  it should "not write when an escaped multiline delimiter is not a closer" in :
    withOriginals: originals =>
      val body: String = "google-album-id = \"\"\"x\\\"\"\"\\\ny\"\"\"\n"
      val file: File = write(originals, "Best.toml", body)
      Album.read(originals, file).googleAlbumId shouldBe "x\"\"\"y"
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.storeGoogle(file, "next", "Title")
      caught.getMessage should include ("google-album-id is not a single line")
      Files.read(file) shouldBe body

  private def roundTrip(title: String, relative: String): Unit =
    val parsed: Title.Parsed = Title.parse(title).getOrElse(fail(title))
    parsed.title shouldBe title
    val originals: File = new File("/originals")
    val file: File = Title.file(originals, parsed)
    Album.relative(originals, file) shouldBe relative
    val directory: String = Album.relative(originals, file.getParentFile)
    val folder: String = if directory == "." then "" else directory
    val stem: String = Files.nameAndExtension(file)._1
    Title.fromLocation(folder, stem).getOrElse(fail(relative)).title shouldBe title

  private def write(originals: File, path: String, content: String): File =
    val file: File = File(originals, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)
    file

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("album-file").toFile
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
