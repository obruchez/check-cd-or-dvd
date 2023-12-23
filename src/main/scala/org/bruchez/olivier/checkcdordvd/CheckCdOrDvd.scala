package org.bruchez.olivier.checkcdordvd

import java.io._
import java.security._

object CheckCdOrDvd {
  def main(args: Array[String]): Unit = {
    if (args.length != 2 && !(args.length == 4 && args(0) == "--move-archived-to")) {
      println("Usage: CheckCdOrDvd [--move-archived-to <dir>] <src> <dst>")
      System.exit(-1)
    } else {
      val (srcDir, dstDir, archivedDirOpt) =
        if (args.length == 4) {
          (args(2), args(3), Some(args(1)))
        } else {
          (args(0), args(1), None)
        }

      compare(
        srcDir = new File(srcDir),
        dstDir = new File(dstDir),
        archivedDirOpt = archivedDirOpt.map(new File(_))
      )
    }
  }

  case class FileWithMd5(file: File, size: Long, md5: String)

  case class FileWithoutMd5(file: File, size: Long) {
    lazy val withMd5: FileWithMd5 = {
      println(s"Computing MD5 for ${file.getCanonicalPath}...")
      val md5 = CheckCdOrDvd.md5(file)
      FileWithMd5(file, size, md5)
    }

    def candidateMatch(that: FileWithoutMd5): Boolean =
      this.file.getName.toLowerCase == that.file.getName.toLowerCase && this.size == that.size
  }

  private def compare(srcDir: File, dstDir: File, archivedDirOpt: Option[File]): Unit = {
    val srcFiles = filesWithoutMd5(srcDir)
    println(s"Source files: ${srcFiles.size}")

    val dstFiles = filesWithoutMd5(dstDir)
    println(s"Destination files: ${dstFiles.size}")

    println()

    val (archivedSrcFiles, nonArchivedSrcFiles) =
      srcFiles partition { srcFile =>
        val dstCandidates = dstFiles.filter(_.candidateMatch(srcFile))
        val archived = dstCandidates.exists(_.withMd5.md5 == srcFile.withMd5.md5)

        if (archived && archivedDirOpt.isDefined) {
          val archivedDir = archivedDirOpt.get
          val archivedFile = new File(
            archivedDir,
            srcFile.file.getAbsolutePath.substring(srcDir.getAbsolutePath.length)
          )
          println(s"Moving ${srcFile.file.getCanonicalPath} to ${archivedFile.getAbsolutePath}...")
          archivedFile.getParentFile.mkdirs()
          srcFile.file.renameTo(archivedFile)
        }

        archived
      }

    val srcPrefixLength = srcDir.getCanonicalPath.length

    def dumpFiles(files: Seq[FileWithoutMd5], header: String): Unit = {
      println(s"=== $header (${files.size}) ===")

      for (file <- files.sortBy(_.file.getCanonicalPath)) {
        println(s" - ${file.file.getCanonicalPath.substring(srcPrefixLength)}")
      }

      println()
    }

    dumpFiles(archivedSrcFiles, "Archived")
    dumpFiles(nonArchivedSrcFiles, "Not archived")
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
