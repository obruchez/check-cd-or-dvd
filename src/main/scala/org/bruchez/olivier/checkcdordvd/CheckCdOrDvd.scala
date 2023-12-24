package org.bruchez.olivier.checkcdordvd

import java.io._
import java.security._

case class Arguments(
    srcDir: File = new File(""),
    dstDir: File = new File(""),
    md5: Boolean = true,
    archivedDirOpt: Option[File] = None
)

object Arguments {
  val NoMd5 = "--no-md5"
  val MoveArchivedTo = "--move-archived-to"

  val usage = s"CheckCdOrDvd [$NoMd5] [$MoveArchivedTo <dir>] <src> <dst>"

  def apply(args: Array[String]): Option[Arguments] =
    this.args(remainingArgs = args, acc = Arguments())

  private def args(remainingArgs: Array[String], acc: Arguments): Option[Arguments] =
    if (remainingArgs.length == 2) {
      Some(acc.copy(srcDir = new File(remainingArgs(0)), dstDir = new File(remainingArgs(1))))
    } else if (remainingArgs.length == 3 && remainingArgs(0) == NoMd5) {
      args(remainingArgs = remainingArgs.drop(1), acc = acc.copy(md5 = false))
    } else if (remainingArgs.length == 4 && remainingArgs(0) == MoveArchivedTo) {
      args(
        remainingArgs = remainingArgs.drop(2),
        acc = acc.copy(archivedDirOpt = Some(new File(remainingArgs(1))))
      )
    } else {
      None
    }
}

object CheckCdOrDvd {

  def main(args: Array[String]): Unit =
    Arguments(args) match {
      case Some(arguments) =>
        compare(arguments)

      case None =>
        println(s"Usage: ${Arguments.usage}")
        System.exit(-1)
    }

  case class FileWithMd5(file: File, size: Long, md5: String)

  case class FileWithoutMd5(file: File, size: Long) {
    lazy val withMd5: FileWithMd5 = {
      println(s"Computing MD5 for ${file.getCanonicalPath}...")
      val md5 = CheckCdOrDvd.md5(file)
      FileWithMd5(file, size, md5)
    }

    def candidateMatch(that: FileWithoutMd5): Boolean = {
      // Same filename and file size
      this.file.getName.toLowerCase == that.file.getName.toLowerCase && this.size == that.size
    }
  }

  private def fileMapping(arguments: Arguments): Seq[(FileWithoutMd5, Option[FileWithoutMd5])] = {
    val srcFiles = filesWithoutMd5(arguments.srcDir)
    println(s"Source files: ${srcFiles.size}")

    val dstFiles = filesWithoutMd5(arguments.dstDir)
    println(s"Destination files: ${dstFiles.size}")

    println()

    srcFiles map { srcFile =>
      val dstCandidates = dstFiles.filter(_.candidateMatch(srcFile))
      val dstFileOpt =
        if (arguments.md5) {
          dstCandidates.find(_.withMd5.md5 == srcFile.withMd5.md5)
        } else {
          dstCandidates.headOption
        }

      for {
        _ <- dstFileOpt
        archiveDir <- arguments.archivedDirOpt
      } {
        val archivedFile = new File(
          archiveDir,
          srcFile.file.getAbsolutePath.substring(arguments.srcDir.getAbsolutePath.length)
        )
        println(s"Moving ${srcFile.file.getCanonicalPath} to ${archivedFile.getAbsolutePath}...")

        archivedFile.getParentFile.mkdirs()
        srcFile.file.renameTo(archivedFile)
      }

      srcFile -> dstFileOpt
    }
  }

  private def compare(arguments: Arguments): Unit = {
    val fileMapping = this.fileMapping(arguments)

    // val (archivedSrcFiles, nonArchivedSrcFiles) =

    val srcPrefixLength = arguments.srcDir.getCanonicalPath.length
    val dstPrefixLength = arguments.dstDir.getCanonicalPath.length

    def dumpFiles(files: Seq[(FileWithoutMd5, Option[FileWithoutMd5])], header: String): Unit = {
      println(s"=== $header (${files.size}) ===")

      for (file <- files.sortBy(_._1.file.getCanonicalPath)) {
        print(s" - ${file._1.file.getCanonicalPath.substring(srcPrefixLength)}")
        file._2.foreach { dstFile =>
          print(s" -> ${dstFile.file.getCanonicalPath.substring(dstPrefixLength)}")
        }
        println()
      }

      println()
    }

    dumpFiles(fileMapping.filter(_._2.isDefined), "Archived")
    dumpFiles(fileMapping.filter(_._2.isEmpty), "Not archived")
  }

  private def filesWithoutMd5(directory: File): Seq[FileWithoutMd5] =
    for {
      file <- filesInDirectory(directory, recursive = true, includeDirectories = false)
      if !fileToIgnore(file)
      size = file.length()
    } yield FileWithoutMd5(file, size)

  private def fileToIgnore(file: File): Boolean = {
    val baseName = file.getName
    Set("Picasa.ini", "Thumbs.db", ".DS_Store").contains(baseName)
  }

  private lazy val md5Digest = MessageDigest.getInstance("MD5")

  def md5(file: File): String = {
    md5Digest.reset()

    val inputStream = new FileInputStream(file)

    try {
      val BufferSize = 4096
      val buffer = new Array[Byte](BufferSize)

      @scala.annotation.tailrec
      def computeMd5UntilEndOfFile(): Unit = {
        val byteCount = inputStream.read(buffer)
        if (byteCount > 0) {
          md5Digest.update(buffer, 0, byteCount)
          computeMd5UntilEndOfFile()
        }
      }

      computeMd5UntilEndOfFile()

      md5Digest.digest().map(byte => f"$byte%02X").mkString
    } finally {
      inputStream.close()
    }
  }

  def filesInDirectory(
      directory: File,
      recursive: Boolean,
      includeDirectories: Boolean
  ): Seq[File] = {
    val (directories, files) =
      Option(directory.listFiles()).fold(Seq[File]())(_.toSeq).partition(_.isDirectory)
    val subDirectoriesAndFiles =
      if (recursive) {
        directories.flatMap(filesInDirectory(_, recursive = true, includeDirectories))
      } else {
        Seq()
      }
    (if (includeDirectories) directories else Seq()) ++ files ++ subDirectoriesAndFiles
  }
}
