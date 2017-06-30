/*
 * Copyright (C) 2009-2011 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray

import shapeless._

package object json {
  type JsField = (String, JsValue)

  def deserializationError(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) = throw new DeserializationException(msg, cause, fieldNames)
  def serializationError(msg: String) = throw new SerializationException(msg)

  implicit class EnrichedAny[T](val any: T) extends AnyVal {
    def toJson(implicit writer: JsonWriter[T]): JsValue = writer.write(any)
  }

  implicit class EnrichedString(val string: String) extends AnyVal {
    def parseJson: JsValue = JsonParser(string)
  }

  // slightly better alternatives to the xError methods above
  @inline
  def deserError[T: Typeable](msg: String, cause: Throwable = null): Nothing =
    throw new DeserializationException(s"deserialising ${Typeable[T].describe}: $msg", cause)

  @inline
  def unexpectedJson[T: Typeable](got: JsValue): Nothing =
    deserializationError(s"unexpected $got")

  @inline
  def serError[T: Typeable](msg: String): Nothing =
    throw new SerializationException(s"serialising ${Typeable[T].describe}: $msg")
}

package json {

  case class DeserializationException(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)
}
