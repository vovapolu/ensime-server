package org.ensime.core

import java.io._

import org.ensime.api._

/**
 * Provides a blocking I/O API for reading and writing to the wire.
 */
trait Protocol {
  def read(input: InputStream): RpcRequestEnvelope
  def write(msg: RpcResponseEnvelope, output: OutputStream): Unit
}
