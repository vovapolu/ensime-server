package org.ensime.server

import org.scalatest._
import java.io._
import org.ensime.api._

class FramedStringProtocolSpec extends FlatSpec with Matchers
    with FramedStringProtocol {

  def read(input: InputStream) = ???
  def write(msg: RpcResponseEnvelope, output: OutputStream): Unit = ???

  "FramedStringProtocol" should "write framed strings" in {
    val out = new ByteArrayOutputStream
    writeString("foobar", out)
    val written = new String(out.toByteArray(), "UTF-8")

    written shouldBe "000006foobar"
  }

  it should "write multi-byte UTF-8 strings" in {
    val out = new ByteArrayOutputStream
    writeString("€", out)
    val written = new String(out.toByteArray(), "UTF-8")

    written shouldBe "000003€"
  }

  it should "read framed strings" in {
    val in = new ByteArrayInputStream("000006foobar".getBytes("UTF-8"))
    val read = readString(in)

    read shouldBe "foobar"
  }

  it should "read multi-byte UTF-8 strings" in {
    val in = new ByteArrayInputStream("000003€".getBytes("UTF-8"))
    val read = readString(in)

    read shouldBe "€"
  }
}
