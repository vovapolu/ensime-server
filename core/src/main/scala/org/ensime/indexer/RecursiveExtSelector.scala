package org.ensime.indexer

import org.apache.commons.vfs2.{ FileSelectInfo, FileSelector }
import java.io.File

private[indexer] abstract class RecursiveExtSelector extends FileSelector {
  def includeFile(info: FileSelectInfo): Boolean =
    include(info.getFile.getName.getExtension)
  def includeFile(f: File): Boolean = include.exists(f.getName.endsWith(_))
  def traverseDescendents(info: FileSelectInfo) = true
  def include: Set[String]
}
