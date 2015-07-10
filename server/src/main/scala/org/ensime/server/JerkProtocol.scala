package org.ensime.server

import java.io._

import org.ensime.api._
import org.ensime.jerk._

import spray.json._

class JerkProtocol extends FramedStringProtocol {
  import JerkEnvelopeFormats._

  override def read(input: InputStream): RpcRequestEnvelope =
    readString(input).parseJson.convertTo[RpcRequestEnvelope]

  override def write(resp: RpcResponseEnvelope, output: OutputStream): Unit =
    writeString(resp.toJson.compactPrint, output)

}
