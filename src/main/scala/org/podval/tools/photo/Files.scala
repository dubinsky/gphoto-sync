package org.podval.tools.photo

import java.io.File
import java.nio.file.Files as NFiles
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
  def deleteEmptyDirectories(directory: File, stop: File): Unit =
    if DirectoryIndex.key(directory) == DirectoryIndex.key(stop) then ()
    else if directory.getParentFile == null then ()
    else if directory.isDirectory && isDirectoryEmpty(directory) then
      NFiles.delete(directory.toPath)
      deleteEmptyDirectories(directory.getParentFile, stop)

  def move(file: File, toFile: File): Unit =
    NFiles.move(file.toPath, toFile.toPath)
  
  def mkDirs(file: File): Unit = file.mkdirs()

  // Not stray: album .toml files, the exact name AGENTS.md, and dotfiles.
  // allowedBesideDirectories is that rule. Any other file fails lsd.
  private def allowedBesideDirectories(file: File): Boolean =
    val name: String = file.getName
    name.endsWith(".toml") || name == "AGENTS.md" || name.startsWith(".")

  def listDirectories(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    val stray: Seq[File] = files.filterNot(allowedBesideDirectories)
    require(stray.isEmpty, stray)
    directories.sortBy(_.getName)

  def listFiles(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    require(directories.isEmpty, directories)
    files

  def list(directory: File): (Seq[File], Seq[File]) = Option(directory.listFiles)
    .map(_.toSeq.partition(_.isDirectory))
    .getOrElse((Seq.empty, Seq.empty))

  def splitOnLast(name: String, char: Char): (String, Option[String]) =
    val index: Int = name.lastIndexOf(char)
    if index == -1
    then (name, None)
    else (name.substring(0, index), Some(name.substring(index + 1)))

  def nameAndExtension(file: File): (String, Option[String]) = splitOnLast(file.getName, '.')

