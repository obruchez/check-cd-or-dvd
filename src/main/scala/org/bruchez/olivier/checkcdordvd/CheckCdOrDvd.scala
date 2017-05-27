package org.bruchez.olivier.checkcdordvd

import java.io._
import java.security._

object CheckCdOrDvd {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: CheckCdOrDvd <src> <dst>")
      System.exit(-1)
    } else {
      compare(new File(args(0)), new File(args(1)))
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

  def compare(sourceDirectory: File, destinationDirectory: File): Unit = {
    val sourceFiles = filesWithoutMd5(sourceDirectory)
    println(s"Source files: ${sourceFiles.size}")

    val destinationFiles = filesWithoutMd5(destinationDirectory)
    println(s"Destination files: ${destinationFiles.size}")

    println()

    val (archivedSourceFiles, nonArchivedSourceFiles) =
      sourceFiles partition { sourceFile =>
        val destinationCandidates = destinationFiles.filter(_.candidateMatch(sourceFile))
        val archived = destinationCandidates.exists(_.withMd5.md5 == sourceFile.withMd5.md5)
        archived
    }

    val sourcePrefixLength = sourceDirectory.getCanonicalPath.length

    def dumpFiles(files: Seq[FileWithoutMd5], header: String): Unit = {
      println(s"=== $header (${files.size}) ===")

      for (file <- files.sortBy(_.file.getCanonicalPath)) {
        println(s" - ${file.file.getCanonicalPath.substring(sourcePrefixLength)}")
      }

      println()
    }

    dumpFiles(archivedSourceFiles, "Archived")
    dumpFiles(nonArchivedSourceFiles, "Not archived")
  }

  def sourceFileFoundInDestinationFiles(sourceFile: FileWithoutMd5, destinationFiles: Seq[FileWithoutMd5]): Unit = {

  }

  def filesWithoutMd5(directory: File): Seq[FileWithoutMd5] =
     for {
       file <- filesInDirectory(directory, recursive = true, includeDirectories = false)
       if !fileToIgnore(file)
       size = file.length()
     } yield FileWithoutMd5(file, size)

  def fileToIgnore(file: File): Boolean = {
    val baseName = file.getName
    Set("Picasa.ini", "Thumbs.db", ".DS_Store").contains(baseName)
  }

  lazy val md5Digest = MessageDigest.getInstance("MD5")

  def md5(file: File): String = {
    md5Digest.reset()

    val inputStream = new FileInputStream(file)

    try {
      val buffer = new Array[Byte](4096)

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

  def filesInDirectory(directory: File, recursive: Boolean, includeDirectories: Boolean): Seq[File] = {
    val (directories, files) = Option(directory.listFiles()).fold(Seq[File]())(_.toSeq).partition(_.isDirectory)
    val subDirectoriesAndFiles =
      if (recursive) directories.flatMap(filesInDirectory(_, recursive = true, includeDirectories)) else Seq()
    (if (includeDirectories) directories else Seq()) ++ files ++ subDirectoriesAndFiles
  }
}
