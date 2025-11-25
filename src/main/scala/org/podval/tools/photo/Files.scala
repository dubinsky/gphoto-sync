package org.podval.tools.photo

import java.io.File
import java.nio.file.attribute.FileTime
import java.nio.file.Files as NFiles
import java.time.Instant
import scala.annotation.tailrec

object Files:
  def read(file: File): String = new String(NFiles.readAllBytes(file.toPath))
  
  def write(string: String, file: File): Unit = NFiles.writeString(file.toPath, string)

  def delete(file: File): Unit =
    if file.exists then
      require(file.isFile)
      file.delete

  private def isDirectoryEmpty(directory: File): Boolean =
    require(directory.isDirectory)
    !NFiles.list(directory.toPath).findFirst.isPresent

  @tailrec
  def deleteEmptyDirectories(directory: File): Unit =
    if isDirectoryEmpty(directory) then
      NFiles.delete(directory.toPath)
      deleteEmptyDirectories(directory.getParentFile)

  def move(file: File, toDirectory: File): Unit =
    val toFile: File = File(toDirectory, file.getName)
    NFiles.move(file.toPath, toFile.toPath)

  def getLastModifiedTime(file: File): Instant =
    //Instant.ofEpochMilli(file.lastModified)
    NFiles.getLastModifiedTime(file.toPath).toInstant

  def setLastModifiedTime(file: File, instant: Instant): Unit =
    //file.setLastModified(getMillis(instant))
    NFiles.setLastModifiedTime(file.toPath, FileTime.from(instant))

  def mkDirs(file: File): Unit = file.mkdirs()

  def listDirectories(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    require(files.isEmpty, files)
    directories

  private def listFiles(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    require(directories.isEmpty, directories)
    files

  def list(directory: File): (Seq[File], Seq[File]) = Option(directory.listFiles)
    .map(_.toSeq.partition(_.isDirectory))
    .getOrElse((Seq.empty, Seq.empty))

  private def nameAndExtension(file: File): (String, String) = splitOnLast(file, '.')

  def splitOnLast(file: File, char: Char): (String, String) =
    val name: String = file.getName
    val index: Int = name.lastIndexOf(char)
    (name.substring(0, index), name.substring(index + 1))

  def group(directory: File): Seq[(String, Set[String])] = listFiles(directory)
    .map(nameAndExtension)
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).toSet)
    .toSeq
    .sortBy(_._1)

  def group(directory: File, name: String): Set[String] =
    group(directory).find(_._1 == name).get._2