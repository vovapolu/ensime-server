package org.ensime.lsp.core

import java.io.{ InputStream, OutputStream }

import akka.event.slf4j.SLF4JLogging
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types._
import org.ensime.lsp.rpc.companions.RpcResponse
import org.ensime.lsp.rpc.messages.JsonRpcMessages.CorrelationId
import org.ensime.lsp.rpc.messages.JsonRpcResponseSuccessMessage
import spray.json.{ DefaultJsonProtocol, FamilyFormats }

object LanguageServer {

  object ResponseHandler extends DefaultJsonProtocol with FamilyFormats {
    def buildHandler(
      langServer: LanguageServer
    ): PartialFunction[(String, ServerCommand, CorrelationId), Option[
      JsonRpcResponseSuccessMessage
    ]] = {
      case (_, InitializeParams(pid, rootPath, capabilities), id) =>
        Some(
          RpcResponse.write(InitializeResult(
                              langServer.initialize(pid, rootPath, capabilities)
                            ),
                            id)
        )
      case ("textDocument/completion",
            TextDocumentCompletionRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        Some(
          RpcResponse
            .write(langServer.completionRequest(textDocument, position), id)
        )
      case ("textDocument/definition",
            TextDocumentDefinitionRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        Some(
          RpcResponse
            .write(langServer.gotoDefinitionRequest(textDocument, position), id)
        )
      case ("textDocument/hover",
            TextDocumentHoverRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        Some(
          RpcResponse.write(langServer.hoverRequest(textDocument, position), id)
        )
      case ("textDocument/documentSymbol", DocumentSymbolParams(tdi), id) =>
        Some(
          RpcResponse
            .write(DocumentSymbolResult(langServer.documentSymbols(tdi)), id)
        )
      case (_, Shutdown(), _) =>
        langServer.shutdown()
        None
    }
  }
}

/**
 * A language server implementation. Users should subclass this class and implement specific behavior.
 */
class LanguageServer(inStream: InputStream, outStream: OutputStream)
    extends SLF4JLogging {

  protected val documentManager = new TextDocumentManager

  private val notificationHandler: Notification => Unit = {
    case DidOpenTextDocumentParams(td) =>
      onOpenTextDocument(td)
    case DidChangeTextDocumentParams(td, changes) =>
      onChangeTextDocument(td, changes)
    case DidSaveTextDocumentParams(td) =>
      onSaveTextDocument(td)
    case DidCloseTextDocumentParams(td) =>
      onCloseTextDocument(td)
    case DidChangeWatchedFiles(changes) =>
      onChangeWatchedFiles(changes)
    case e => log.error(s"Unknown notification $e")
  }

  val connection: Connection = new Connection(
    inStream,
    outStream,
    Seq(documentManager.notificationHandler, notificationHandler),
    (method, command, id) =>
      LanguageServer.ResponseHandler
        .buildHandler(this)
        .lift
        .andThen(_.getOrElse({
          log.error(s"Unknown command $method $command $id")
          sys.error("Unknown command")
        }))((method, command, id))
  )

  def start(): Unit =
    connection.start()

  def onOpenTextDocument(td: TextDocumentItem): Unit =
    log.debug(s"openTextDocument $td")

  def onChangeTextDocument(td: VersionedTextDocumentIdentifier,
                           changes: Seq[TextDocumentContentChangeEvent]): Unit =
    log.debug(s"changeTextDocument $td")

  def onSaveTextDocument(td: TextDocumentIdentifier): Unit = {
    log.debug(s"saveTextDocument $td")
    connection.showMessage(MessageType.Info, s"Saved text document ${td.uri}")
  }

  def onCloseTextDocument(td: TextDocumentIdentifier): Unit =
    log.debug(s"closeTextDocument $td")

  def onChangeWatchedFiles(changes: Seq[FileEvent]): Unit =
    log.debug(s"changeWatchedFiles $changes")

  def initialize(pid: Long,
                 rootPath: String,
                 capabilities: ClientCapabilities): ServerCapabilities = {
    log.info(s"Initialized with $pid, $rootPath, $capabilities")
    ServerCapabilities(
      completionProvider = Some(CompletionOptions(false, Seq(".")))
    )
  }

  def completionRequest(textDocument: TextDocumentIdentifier,
                        position: Position): ResultResponse =
    CompletionList(isIncomplete = false, Nil)

  def shutdown(): Unit = {}

  def gotoDefinitionRequest(textDocument: TextDocumentIdentifier,
                            position: Position): DefinitionResult =
    DefinitionResult(Seq.empty[Location])

  def hoverRequest(textDocument: TextDocumentIdentifier,
                   position: Position): Hover =
    Hover(Nil, None)

  def documentSymbols(tdi: TextDocumentIdentifier): Seq[SymbolInformation] =
    Seq.empty
}
