// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import spray.json._

object JsonUtils {
  def wrapperFormat[A, B: JsonFormat](wrap: B => A,
                                      unwrap: A => B): JsonFormat[A] =
    new JsonFormat[A] {
      def write(obj: A): JsValue = unwrap(obj).toJson
      def read(j: JsValue): A    = wrap(j.convertTo[B])
    }
}
