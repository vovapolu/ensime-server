package org.ensime.lsp.api.companions

import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.api.commands._
import spray.json._
import shapeless._

private object CompanionHelper {
  def wrapperFormat[A, B: JsonFormat](wrap: B => A,
                                      unwrap: A => B): JsonFormat[A] =
    new JsonFormat[A] {
      def write(obj: A): JsValue = unwrap(obj).toJson
      def read(j: JsValue): A = wrap(j.convertTo[B])
    }
}

private object ServerConversions
    extends DefaultJsonProtocol
    with FamilyFormats {

  import org.ensime.lsp.api.TypesFormat._

  implicit val textDocumentPositionParamsFormat
    : JsonFormat[TextDocumentPositionParams] =
    cachedImplicit
  val initializeParamsFormat: JsonFormat[InitializeParams] = cachedImplicit
  val shutdownFormat: JsonFormat[Shutdown] = cachedImplicit

  val textDocumentCompletionRequestFormat
    : JsonFormat[TextDocumentCompletionRequest] =
    CompanionHelper.wrapperFormat(TextDocumentCompletionRequest, _.params)
  val textDocumentDefinitionRequestFormat
    : JsonFormat[TextDocumentDefinitionRequest] =
    CompanionHelper.wrapperFormat(TextDocumentDefinitionRequest, _.params)
  val textDocumentHoverRequestFormat: JsonFormat[TextDocumentHoverRequest] =
    CompanionHelper.wrapperFormat(TextDocumentHoverRequest, _.params)

  val documentSymbolParamsFormat: JsonFormat[DocumentSymbolParams] =
    cachedImplicit
}

object ServerCommand extends CommandCompanion[ServerCommand] {

  import ServerConversions._

  implicit val initializeCommand: Command[InitializeParams] =
    Command("initialize", initializeParamsFormat)
  implicit val shutdownCommand: Command[Shutdown] =
    Command("shutdown", shutdownFormat)
  implicit val competitionCommand: Command[TextDocumentCompletionRequest] =
    Command("textDocument/completion", textDocumentCompletionRequestFormat)
  implicit val definitionCommand: Command[TextDocumentDefinitionRequest] =
    Command("textDocument/definition", textDocumentDefinitionRequestFormat)
  implicit val hoverCommand: Command[TextDocumentHoverRequest] =
    Command("textDocument/hover", textDocumentHoverRequestFormat)
  implicit val documentSymbolCommand: Command[DocumentSymbolParams] =
    Command("textDocument/documentSymbol", documentSymbolParamsFormat)

  val commands = Seq(
    initializeCommand,
    shutdownCommand,
    competitionCommand,
    definitionCommand,
    hoverCommand,
    documentSymbolCommand
  )
}

private object ClientConversions
    extends DefaultJsonProtocol
    with FamilyFormats {
  val showMessageRequestParamsFormat: JsonFormat[ShowMessageRequestParams] =
    cachedImplicit
}

object ClientCommand extends CommandCompanion[ClientCommand] {
  implicit val showMessageRequestCommand: Command[ShowMessageRequestParams] =
    Command("showMessageRequest",
            ClientConversions.showMessageRequestParamsFormat)

  val commands = Seq(showMessageRequestCommand)
}

object ClientNotificationConversions
    extends DefaultJsonProtocol
    with FamilyFormats {

  import org.ensime.lsp.api.TypesFormat._

  val showMessageParamsFormat: JsonFormat[ShowMessageParams] =
    cachedImplicit
  val logMessageParamsFormat: JsonFormat[LogMessageParams] =
    cachedImplicit
  val publishDiagnosticsFormat: JsonFormat[PublishDiagnostics] =
    cachedImplicit
  val didOpenTextDocumentParamsFormat: JsonFormat[DidOpenTextDocumentParams] =
    cachedImplicit
  val didChangeTextDocumentParamsFormat
    : JsonFormat[DidChangeTextDocumentParams] =
    cachedImplicit
  val didCloseTextDocumentParamsFormat: JsonFormat[DidCloseTextDocumentParams] =
    cachedImplicit
  val didSaveTextDocumentParamsFormat: JsonFormat[DidSaveTextDocumentParams] =
    cachedImplicit
  val didChangeWatchedFilesFormat: JsonFormat[DidChangeWatchedFiles] =
    cachedImplicit
  val initializedFormat: JsonFormat[Initialized] =
    cachedImplicit
  val cancelRequestFormat: JsonFormat[CancelRequest] =
    cachedImplicit
}

object ClientNotification extends NotificationCompanion[ClientNotification] {

  import ClientNotificationConversions._

  implicit val showMessageNotification: Notification[ShowMessageParams] =
    Notification("window/showMessage", showMessageParamsFormat)
  implicit val logMessageNotification: Notification[LogMessageParams] =
    Notification("window/logMessage", logMessageParamsFormat)
  implicit val publishDiagnosticsNotification
    : Notification[PublishDiagnostics] =
    Notification("textDocument/publishDiagnostics", publishDiagnosticsFormat)
  implicit val didOpenNotification: Notification[DidOpenTextDocumentParams] =
    Notification("textDocument/didOpen", didOpenTextDocumentParamsFormat)
  implicit val didChangeNotification
    : Notification[DidChangeTextDocumentParams] =
    Notification("textDocument/didChange", didChangeTextDocumentParamsFormat)
  implicit val didCloseNotification: Notification[DidCloseTextDocumentParams] =
    Notification("textDocument/didClose", didCloseTextDocumentParamsFormat)
  implicit val didSaveNotification: Notification[DidSaveTextDocumentParams] =
    Notification("textDocument/didSave", didSaveTextDocumentParamsFormat)
  implicit val didChangeWatchedFilesNotification
    : Notification[DidChangeWatchedFiles] =
    Notification("workspace/didChangeWatchedFiles", didChangeWatchedFilesFormat)
  implicit val initializedNotification: Notification[Initialized] =
    Notification("initialized", initializedFormat)
  implicit val cancelRequestNotification: Notification[CancelRequest] =
    Notification("$/cancelRequest", cancelRequestFormat)

  val notifications = Seq(
    showMessageNotification,
    logMessageNotification,
    publishDiagnosticsNotification,
    didOpenNotification,
    didChangeNotification,
    didCloseNotification,
    didSaveNotification,
    didChangeWatchedFilesNotification,
    initializedNotification,
    cancelRequestNotification
  )

}

//object ResultResponse extends ResponseCompanion[Any] {
//
//  override val ResponseFormats = Message.MessageFormats(
//    "initialize" -> Json.format[InitializeResult],
//    "textDocument/completion" -> Json.format[CompletionList],
//    "textDocument/definition" -> valueFormat(DefinitionResult)(_.params),
//    "textDocument/hover" -> Json.format[Hover],
//    "textDocument/documentSymbol" -> valueFormat(DocumentSymbolResult)(_.params),
//    "shutdown" -> Json.format[ShutdownResult])
//}
