package org.ensime.lsp.api

import spray.json._
import types._

import scala.util.Try

private object TypesConversions extends DefaultJsonProtocol with FamilyFormats {

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
}
