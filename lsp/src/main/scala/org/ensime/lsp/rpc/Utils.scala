package org.ensime.lsp.rpc

import spray.json._

object Utils {
  def wrapperFormat[A, B: JsonFormat](wrap: B => A,
                                      unwrap: A => B): JsonFormat[A] =
    new JsonFormat[A] {
      def write(obj: A): JsValue = unwrap(obj).toJson
      def read(j: JsValue): A    = wrap(j.convertTo[B])
    }
}
