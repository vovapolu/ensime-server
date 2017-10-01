package org.ensime.lsp.api

object TextDocumentSyncKind {
  /**
    * Documents should not be synced at all.
    */
  final val None = 0

  /**
    * Documents are synced by always sending the full content
    * of the document.
    */
  final val Full = 1

  /**
    * Documents are synced by sending the full content on open.
    * After that only incremental updates to the document are
    * send.
    */
  final val Incremental = 2
}

object MessageType {
  /** An error message. */
  final val Error = 1

  /** A warning message. */
  final val Warning = 2

  /** An information message. */
  final val Info = 3

  /** A log message. */
  final val Log = 4
}

sealed trait Message
sealed trait ServerCommand extends Message
sealed trait ClientCommand extends Message

sealed trait Response extends Message
sealed trait ResultResponse extends Response

sealed trait Notification extends Message

/**
 * Parameters and types used in the `initialize` message.
 */
case class InitializeParams(
  /**
   * The process Id of the parent process that started
   * the server.
   */
  processId: Long,

  /**
   * The rootPath of the workspace. Is null
   * if no folder is open.
   */
  rootPath: String,

  /**
   * The capabilities provided by the client (editor)
   */
  capabilities: ClientCapabilities) extends ServerCommand

case class InitializeError(retry: Boolean)

case class ClientCapabilities()

//object ClientCapabilities {
//  implicit val format: Format[ClientCapabilities] = Format(
//    Reads(jsValue => JsSuccess(ClientCapabilities())),
//    Writes(c => Json.obj()))
//} // FIXME

case class ServerCapabilities(
  /**
   * Defines how text documents are synced.
   */
  textDocumentSync: Int = TextDocumentSyncKind.Full,
  /**
   * The server provides hover support.
   */
  hoverProvider: Boolean = false,
  /**
   * The server provides completion support.
   */
  completionProvider: Option[CompletionOptions],
  /**
   * The server provides signature help support.
   */
  signatureHelpProvider: Option[SignatureHelpOptions] = None,
  /**
   * The server provides goto definition support.
   */
  definitionProvider: Boolean = false,
  /**
   * The server provides find references support.
   */
  referencesProvider: Boolean = false,
  /**
   * The server provides document highlight support.
   */
  documentHighlightProvider: Boolean = false,
  /**
   * The server provides document symbol support.
   */
  documentSymbolProvider: Boolean = false,
  /**
   * The server provides workspace symbol support.
   */
  workspaceSymbolProvider: Boolean = false,
  /**
   * The server provides code actions.
   */
  codeActionProvider: Boolean = false,
  /**
   * The server provides code lens.
   */
  codeLensProvider: Option[CodeLensOptions] = None,
  /**
   * The server provides document formatting.
   */
  documentFormattingProvider: Boolean = false,
  /**
   * The server provides document range formatting.
   */
  documentRangeFormattingProvider: Boolean = false,
  /**
   * The server provides document formatting on typing.
   */
  documentOnTypeFormattingProvider: Option[DocumentOnTypeFormattingOptions] = None,
  /**
   * The server provides rename support.
   */
  renameProvider: Boolean = false
)

case class CompletionOptions(resolveProvider: Boolean, triggerCharacters: Seq[String])

case class SignatureHelpOptions(triggerCharacters: Seq[String])

case class CodeLensOptions(resolveProvider: Boolean = false)

case class DocumentOnTypeFormattingOptions(firstTriggerCharacter: String, moreTriggerCharacters: Seq[String])

case class CompletionList(isIncomplete: Boolean, items: Seq[CompletionItem]) extends ResultResponse

case class InitializeResult(capabilities: ServerCapabilities) extends ResultResponse

case class Shutdown() extends ServerCommand
//object Shutdown {
//  implicit val format: Format[Shutdown] = OFormat(
//    Reads(jsValue => JsSuccess(Shutdown())),
//    OWrites[Shutdown](s => Json.obj()))
//}

case class ShutdownResult(dummy: Int) extends ResultResponse

case class ShowMessageRequestParams(
  /**
   * The message type. @see MessageType
   */
  tpe: Long,

  /**
   * The actual message
   */
  message: String,

  /**
   * The message action items to present.
   */
  actions: Seq[MessageActionItem]) extends ClientCommand

/**
 * A short title like 'Retry', 'Open Log' etc.
 */
case class MessageActionItem(title: String)

case class TextDocumentPositionParams(textDocument: TextDocumentIdentifier, position: Position)
case class DocumentSymbolParams(textDocument: TextDocumentIdentifier) extends ServerCommand

case class TextDocumentCompletionRequest(params: TextDocumentPositionParams) extends ServerCommand
case class TextDocumentDefinitionRequest(params: TextDocumentPositionParams) extends ServerCommand
case class TextDocumentHoverRequest(params: TextDocumentPositionParams) extends ServerCommand

case class Hover(contents: Seq[MarkedString], range: Option[Range]) extends ResultResponse

//object ServerCommand extends CommandCompanion[ServerCommand] {
//
//  implicit val positionParamsFormat = Json.format[TextDocumentPositionParams]
//
//  override val CommandFormats = Message.MessageFormats(
//    "initialize" -> Json.format[InitializeParams],
//    "shutdown" -> Shutdown.format,
//    "textDocument/completion" -> valueFormat(TextDocumentCompletionRequest)(_.params),
//    "textDocument/definition" -> valueFormat(TextDocumentDefinitionRequest)(_.params),
//    "textDocument/hover" -> valueFormat(TextDocumentHoverRequest)(_.params),
//    "textDocument/documentSymbol" -> Json.format[DocumentSymbolParams]
//  )
//}
//
//object ClientCommand extends CommandCompanion[ClientCommand] {
//  override val CommandFormats = Message.MessageFormats(
//    "window/showMessageRequest" -> Json.format[ShowMessageRequestParams])
//}

///////////////////////////// Notifications ///////////////////////////////

// From server to client

case class ShowMessageParams(tpe: Long, message: String) extends Notification
case class LogMessageParams(tpe: Long, message: String) extends Notification
case class PublishDiagnostics(uri: String, diagnostics: Seq[Diagnostic]) extends Notification

// from client to server

case class ExitNotification() extends Notification
case class DidOpenTextDocumentParams(textDocument: TextDocumentItem) extends Notification
case class DidChangeTextDocumentParams(
  textDocument: VersionedTextDocumentIdentifier,
  contentChanges: Seq[TextDocumentContentChangeEvent]) extends Notification

case class DidCloseTextDocumentParams(textDocument: TextDocumentIdentifier) extends Notification
case class DidSaveTextDocumentParams(textDocument: TextDocumentIdentifier) extends Notification
case class DidChangeWatchedFiles(changes: Seq[FileEvent]) extends Notification

case class Initialized() extends Notification
//object Initialized {
//  implicit val format: Format[Initialized] = OFormat(
//    Reads(jsValue => JsSuccess(Initialized())),
//    OWrites[Initialized](s => Json.obj()))
//}


case class CancelRequest(id: Int) extends Notification

case class FileEvent(uri: String, `type`: Int)

object FileChangeType {
  final val Created = 1
  final val Changed = 2
  final val Deleted = 3
}

//object Notification extends NotificationCompanion[Notification] {
//  override val NotificationFormats = Message.MessageFormats(
//    "window/showMessage" -> Json.format[ShowMessageParams],
//    "window/logMessage" -> Json.format[LogMessageParams],
//    "textDocument/publishDiagnostics" -> Json.format[PublishDiagnostics],
//    "textDocument/didOpen" -> Json.format[DidOpenTextDocumentParams],
//    "textDocument/didChange" -> Json.format[DidChangeTextDocumentParams],
//    "textDocument/didClose" -> Json.format[DidCloseTextDocumentParams],
//    "textDocument/didSave" -> Json.format[DidSaveTextDocumentParams],
//    "workspace/didChangeWatchedFiles" -> Json.format[DidChangeWatchedFiles],
//    "initialized" -> Initialized.format,
//    "$/cancelRequest" -> Json.format[CancelRequest]
//  )
//}

case class DocumentSymbolResult(params: Seq[SymbolInformation]) extends ResultResponse
case class DefinitionResult(params: Seq[Location]) extends ResultResponse

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
