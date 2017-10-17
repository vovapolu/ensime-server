package org.ensime.lsp.core

import java.io.{ PipedInputStream, PipedOutputStream, PrintWriter }

import org.scalatest._
import org.scalatest.Matchers._

class MessageReaderSpec extends FreeSpec with BeforeAndAfter {

  var inStream: PipedInputStream = _
  var out: PrintWriter           = _

  def writeToStream(msg: String): Unit = {
    out.print(msg.replaceAll("\n", "\r\n"))
    out.flush()
  }

  before {
    inStream = new PipedInputStream
    out = new PrintWriter(new PipedOutputStream(inStream))
  }

  after {
    out.close()
    // inStream.close()
    // it causes java.io.IOException: Pipe closed ¯\_(ツ)_/¯
  }

  "full headers" - {
    "should be processed correctly" in {
      val msgReader = new MessageReader(inStream)
      writeToStream("""
                      |Content-Length: 80
                      |Content-Type: application/vscode-jsonrpc; charset=utf8
                      |
                      |""".stripMargin)
      val headers = msgReader.getHeaders

      headers("Content-Length") shouldEqual "80"
      headers("Content-Type") shouldEqual "application/vscode-jsonrpc; charset=utf8"
    }
  }

  "partial headers" - {
    "should be concatenated correctly" in {
      val msgReader = new MessageReader(inStream)

      writeToStream("""|Content-Length: 80
                       |Content""".stripMargin)
      writeToStream("""|-Type: application/vscode-jsonrpc; charset=utf8
                       |
                  """.stripMargin)
      val headers = msgReader.getHeaders

      headers("Content-Length") shouldEqual "80"
      headers("Content-Type") shouldEqual "application/vscode-jsonrpc; charset=utf8"
    }
  }

  "multi-chunk header" - {
    "should be processed correctly" in {
      val msgReader = new MessageReader(inStream)

      writeToStream("""Content-""")
      writeToStream("""Length: 80""")
      Thread.sleep(100)
      writeToStream("""|
                       |Content-Type: application/vscode-jsonrpc; charset=utf8
                       |
                       |""".stripMargin)
      val headers = msgReader.getHeaders

      headers("Content-Length") shouldEqual "80"
      headers("Content-Type") shouldEqual "application/vscode-jsonrpc; charset=utf8"
    }
  }

  "payload arriving" - {
    "should be processed correctly" in {
      val msgReader = new MessageReader(inStream)
      writeToStream(
        """|Content-Length: 43
           |
           |{"jsonrpc":"2.0","id":1,"method":"example"}""".stripMargin
      )

      val payload = msgReader.nextPayload()

      payload shouldEqual Some(
        """{"jsonrpc":"2.0","id":1,"method":"example"}"""
      )
    }
  }

  "chunked payload arriving" - {
    "should be processed correctly" in {
      val msgReader = new MessageReader(inStream)
      writeToStream("""|Content-Length: 43
                       |
                       |{"jsonrpc":"2.0",""".stripMargin)
      writeToStream(""""id":1,"method":"example"}""")

      val payload = msgReader.nextPayload()

      payload shouldEqual Some(
        """{"jsonrpc":"2.0","id":1,"method":"example"}"""
      )
    }
  }
}
