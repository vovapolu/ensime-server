package org.ensime.lsp

import java.util.concurrent.{ ConcurrentHashMap, ConcurrentMap }

import akka.event.slf4j.SLF4JLogging
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types._

import scala.collection.JavaConverters._

/**
 * A class to manage text documents coming over the wire from a Language Server client.
 *
 * The manager keeps an up to date version of each document that is currently open by the client.
 */
class TextDocumentManager extends SLF4JLogging {
  val notificationHandler: Notification => Unit = {
    case DidOpenTextDocumentParams(td) =>
      onOpenTextDocument(td)
    case DidChangeTextDocumentParams(td, changes) =>
      onChangeTextDocument(td, changes)
    case DidCloseTextDocumentParams(td) =>
      onCloseTextDocument(td)
    case e => ()
  }

  private val docs: ConcurrentMap[String, TextDocument] = new ConcurrentHashMap

  def documentForUri(uri: String): Option[TextDocument] = Option(docs.get(uri))

  def allOpenDocuments: Seq[TextDocument] = docs.values.asScala.toSeq

  def onOpenTextDocument(td: TextDocumentItem): Unit =
    docs.put(td.uri, TextDocument(td.uri, td.text.toCharArray))

  def onChangeTextDocument(td: VersionedTextDocumentIdentifier,
                           changes: Seq[TextDocumentContentChangeEvent]): Unit =
    documentForUri(td.uri) match {
      case None =>
        log.error(s"Document ${td.uri} not found in this manager. Adding now")
        // we assume full text sync
        docs.put(td.uri, TextDocument(td.uri, changes.head.text.toCharArray))
      case Some(doc) =>
        docs.put(td.uri, doc.applyChanges(changes))
    }

  def onCloseTextDocument(td: TextDocumentIdentifier): Unit =
    docs.remove(td.uri)

}
