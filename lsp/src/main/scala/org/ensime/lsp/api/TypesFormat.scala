package org.ensime.lsp.api

import spray.json._
import types._
import shapeless._

import scala.util.Try

private object TypesConversions extends DefaultJsonProtocol with FamilyFormats {
  implicit val positionFormat: JsonFormat[Position]     = cachedImplicit
  implicit val rangeFormat: JsonFormat[Range]           = cachedImplicit
  implicit val locationFormat: JsonFormat[Location]     = cachedImplicit
  implicit val diagnosticFormat: JsonFormat[Diagnostic] = cachedImplicit
  implicit val textDocumentIdentifierFormat
    : JsonFormat[TextDocumentIdentifier] = cachedImplicit
  implicit val versionedTextDocumentIdentifierFormat
    : JsonFormat[VersionedTextDocumentIdentifier] = cachedImplicit
  implicit val textDocumentItemFormat: JsonFormat[TextDocumentItem] =
    cachedImplicit
  implicit val completionItemFormat: JsonFormat[CompletionItem] = cachedImplicit
  implicit val rawMarkedStringFormat: JsonFormat[RawMarkedString] =
    cachedImplicit
  implicit val markdownStringFormat: JsonFormat[MarkdownString] = cachedImplicit

  implicit object markedStringFormat extends JsonFormat[MarkedString] {
    def read(j: JsValue): MarkedString =
      (Try(j.convertTo[RawMarkedString]) orElse
        Try(j.convertTo[MarkdownString])).fold(
        e => sys.error(s"Wrong MarkedString format for $j"),
        x => x
      )
    override def write(obj: MarkedString): JsValue = obj match {
      case raw: RawMarkedString     => raw.toJson
      case markdown: MarkdownString => markdown.toJson
    }
  }

  implicit val symbolInformationFormat: JsonFormat[SymbolInformation] =
    cachedImplicit
  implicit val textDocumentContentChangeEventFormat
    : JsonFormat[TextDocumentContentChangeEvent] = cachedImplicit
}

object TypesFormat {
  implicit val positionFormat: JsonFormat[Position] =
    TypesConversions.positionFormat
  implicit val rangeFormat: JsonFormat[Range] =
    TypesConversions.rangeFormat
  implicit val locationFormat: JsonFormat[Location] =
    TypesConversions.locationFormat
  implicit val diagnosticFormat: JsonFormat[Diagnostic] =
    TypesConversions.diagnosticFormat
  implicit val textDocumentIdentifierFormat
    : JsonFormat[TextDocumentIdentifier] =
    TypesConversions.textDocumentIdentifierFormat
  implicit val versionedTextDocumentIdentifierFormat
    : JsonFormat[VersionedTextDocumentIdentifier] =
    TypesConversions.versionedTextDocumentIdentifierFormat
  implicit val textDocumentItemFormat: JsonFormat[TextDocumentItem] =
    TypesConversions.textDocumentItemFormat
  implicit val completionItemFormat: JsonFormat[CompletionItem] =
    TypesConversions.completionItemFormat
  implicit val rawMarkedStringFormat: JsonFormat[RawMarkedString] =
    TypesConversions.rawMarkedStringFormat
  implicit val markdownStringFormat: JsonFormat[MarkdownString] =
    TypesConversions.markdownStringFormat
  implicit val markedStringFormat: JsonFormat[MarkedString] =
    TypesConversions.markedStringFormat
  implicit val symbolInformationFormat: JsonFormat[SymbolInformation] =
    TypesConversions.symbolInformationFormat
  implicit val textDocumentContentChangeEventFormat
    : JsonFormat[TextDocumentContentChangeEvent] =
    TypesConversions.textDocumentContentChangeEventFormat
}
