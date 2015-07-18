package org.ensime.server

import java.io._

import org.ensime.sexp._
import org.ensime.server.protocol.swank._
import org.ensime.api._
import org.ensime.core.Protocol

class SwankProtocol extends FramedStringProtocol {
  import SwankFormats._

  // can throw if the message is bad
  override def read(input: InputStream): RpcRequestEnvelope =
    readString(input).parseSexp.convertTo[RpcRequestEnvelope]

  override def write(resp: RpcResponseEnvelope, output: OutputStream): Unit = {
    writeString(resp.toSexp.prettyPrint, output)
  }

}
