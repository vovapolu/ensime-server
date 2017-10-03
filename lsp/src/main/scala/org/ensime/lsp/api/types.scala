package org.ensime.lsp.api.types

/**
 * Position in a text document expressed as zero-based line and character offset.
 */
case class Position(line: Int, character: Int)

/**
 * A range in a text document.
 */
case class Range(start: Position, end: Position)

/**
 * Represents a location inside a resource, such as a line
 * inside a text file.
 */
case class Location(uri: String, range: Range)

object DiagnosticSeverity {
  final val Error       = 1
  final val Warning     = 2
  final val Information = 3
  final val Hint        = 4
}

case class Diagnostic(
  range: Range, // the range at which this diagnostic applies
  severity: Option[Int], // severity of this diagnostics (see above)
  code: Option[String], // a code for this diagnostic
  source: Option[String], // the source of this diagnostic (like 'typescript' or 'scala')
  message: String // the diagnostic message
)

/**
 * A reference to a command.
 *
 * @param title The title of the command, like 'Save'
 * @param command The identifier of the actual command handler
 * @param arguments The arugments this command may be invoked with
 */
case class Command(title: String, command: String, arguments: Seq[Any])

case class TextEdit(range: Range, newText: String)

/**
 * A workspace edit represents changes to many resources managed
 * in the workspace.
 */
case class WorkspaceEdit(changes: Map[String, Seq[TextEdit]]) // uri -> changes

case class TextDocumentIdentifier(uri: String)

case class VersionedTextDocumentIdentifier(uri: String, version: Long)

/**
 * An item to transfer a text document from the client to the
 * server.
 */
case class TextDocumentItem(uri: String,
                            languageId: String,
                            /**
                             * The version number of this document (it will strictly increase after each
                             * change, including undo/redo).
                             */
                            version: Long,
                            text: String)

object CompletionItemKind {
  final val Text        = 1
  final val Method      = 2
  final val Function    = 3
  final val Constructor = 4
  final val Field       = 5
  final val Variable    = 6
  final val Class       = 7
  final val Interface   = 8
  final val Module      = 9
  final val Property    = 10
  final val Unit        = 11
  final val Value       = 12
  final val Enum        = 13
  final val Keyword     = 14
  final val Snippet     = 15
  final val Color       = 16
  final val File        = 17
  final val Reference   = 18
}

case class CompletionItem(
  label: String,
  kind: Option[Int] = None,
  detail: Option[String] = None,
  documentation: Option[String] = None,
  sortText: Option[String] = None,
  filterText: Option[String] = None,
  insertText: Option[String] = None,
  textEdit: Option[String] = None,
  data: Option[String] = None
) // An data entry field that is preserved on a completion item between
// a [CompletionRequest](#CompletionRequest) and a [CompletionResolveRequest]
//   (#CompletionResolveRequest)

sealed trait MarkedString

case class RawMarkedString(language: String, value: String)
    extends MarkedString {
  def this(value: String) { // why not a factory constructor
    this("text", value)
  }
}

case class MarkdownString(contents: String) extends MarkedString

object MarkedString {
  import org.ensime.lsp.api.TypesConversions
  import spray.json.JsonFormat

  implicit val markedStringFormat: JsonFormat[MarkedString] =
    TypesConversions.markedStringFormat
}

case class ParameterInformation(label: String, documentation: Option[String])

case class SignatureInformation(label: String,
                                documentation: Option[String],
                                parameters: Seq[ParameterInformation])

/**
 * Signature help represents the signature of something
 * callable. There can be multiple signature but only one
 * active and only one active parameter.
 */
case class SignatureHelp(
                         /** One or more signatures. */
                         signatures: Seq[SignatureInformation],
                         /** The active signature. */
                         activeSignature: Option[Int],
                         /** The active parameter of the active signature. */
                         activeParameter: Option[Int])

/**
 * Value-object that contains additional information when
 * requesting references.
 */
case class ReferenceContext(
  /** Include the declaration of the current symbol. */
  includeDeclaration: Boolean
)

object DocumentHighlightKind {

  /**
   * A textual occurrence.
   */
  final val Text = 1

  /**
   * Read-access of a symbol, like reading a variable.
   */
  final val Read = 2

  /**
   * Write-access of a symbol, like writing to a variable.
   */
  final val Write = 3
}

/**
 * A document highlight is a range inside a text document which deserves
 * special attention. Usually a document highlight is visualized by changing
 * the background color of its range.
 */
case class DocumentHighlight(
                             /** The range this highlight applies to. */
                             range: Range,
                             /** The highlight kind, default is [text](#DocumentHighlightKind.Text). */
                             kind: Int = DocumentHighlightKind.Text)

object SymbolKind {
  final val File        = 1
  final val Module      = 2
  final val Namespace   = 3
  final val Package     = 4
  final val Class       = 5
  final val Method      = 6
  final val Property    = 7
  final val Field       = 8
  final val Constructor = 9
  final val Enum        = 10
  final val Interface   = 11
  final val Function    = 12
  final val Variable    = 13
  final val Constant    = 14
  final val String      = 15
  final val Number      = 16
  final val Boolean     = 17
  final val Array       = 18
}

case class SymbolInformation(name: String,
                             kind: Int,
                             location: Location,
                             containerName: Option[String])

/**
 * The parameters of a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
 */
case class WorkspaceSymbolParams(query: String)

case class CodeActionContext(diagnostics: Seq[Diagnostic])

/**
 * A code lens represents a [command](#Command) that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For performance
 * reasons the creation of a code lens and resolving should be done to two stages.
 */
case class CodeLens(
                    /**
                     * The range in which this code lens is valid. Should only span a single line.
                     */
                    range: Range,
                    /**
                     * The command this code lens represents.
                     */
                    command: Option[Command],
                    /**
                     * An data entry field that is preserved on a code lens item between
                     * a [CodeLensRequest](#CodeLensRequest) and a [CodeLensResolveRequest]
                     * (#CodeLensResolveRequest)
                     */
                    data: Option[Any])

/**
 * Value-object describing what options formatting should use.
 */
case class FormattingOptions(
  /**
   * Size of a tab in spaces.
   */
  tabSize: Int,
  /**
   * Prefer spaces over tabs.
   */
  insertSpaces: Boolean,
  /**
   * Signature for further properties.
   */
  params: Map[String, Any] // [key: string]: boolean | number | string;
)

/**
 * An event describing a change to a text document. If range and rangeLength are omitted
 * the new text is considered to be the full content of the document.
 */
case class TextDocumentContentChangeEvent(
  /**
   * The range of the document that changed.
   */
  range: Option[Range],
  /**
   * The length of the range that got replaced.
   */
  rangeLength: Option[Int],
  /**
   * The new text of the document.
   */
  text: String
)
