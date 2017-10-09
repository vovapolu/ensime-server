package org.ensime.lsp.ensime

import java.net.URI
import java.nio.file._

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._

import scala.util.{ Success, Try }

/**
 * A place to unzip files from archives
 */
class TempFileStore(val path: String) extends SLF4JLogging {
  val rootPath: Path = FileSystems.getDefault.getPath(path)

  if (!Files.exists(rootPath)) {
    Files.createDirectory(rootPath)
  }

  assert(Files.isDirectory(rootPath), s"File store $path is not a directory")

  def getFile(path: EnsimeFile): Try[Path] = path match {
    case RawFile(p) => Success(p)
    case ArchiveFile(jar, entry) =>
      Try {
        log.info(s"Extracting $jar!$entry to $rootPath")
        val uri = URI.create(s"jar:${jar.toFile.toURI.toString}")
        val zipFile =
          FileSystems.newFileSystem(uri, new java.util.HashMap[String, String])
        val zipFilePath   = zipFile.getPath(entry)
        val targetPath    = if (entry.startsWith("/")) entry.drop(1) else entry
        val extractedPath = rootPath.resolve(targetPath)

        try {
          Files.createDirectories(extractedPath.getParent)
          Files.copy(zipFilePath,
                     extractedPath,
                     StandardCopyOption.REPLACE_EXISTING)
        } finally zipFile.close()

        extractedPath
      }
  }
}
