// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

package object sexp {
  implicit def pimpAny[T](any: T): PimpedAny[T] = new PimpedAny(any)
  implicit def pimpString(string: String): PimpedString = new PimpedString(string)
}

package sexp {
  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

  private[sexp] class PimpedAny[T](any: T) {
    def toSexp(implicit writer: SexpWriter[T]): Sexp = writer.write(any)
  }

  private[sexp] class PimpedString(string: String) {
    def parseSexp: Sexp = SexpParser.parse(string)
  }
}
