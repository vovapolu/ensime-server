package org.ensime.server

import java.io._

import akka.event.slf4j.SLF4JLogging

import org.ensime.api._
import org.ensime.core.Protocol

// stopgap until we enable WebSockets
trait FramedStringProtocol extends Protocol with SLF4JLogging {

  protected def writeString(value: String, out: OutputStream): Unit = {
    val data: Array[Byte] = value.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")

    if (log.isTraceEnabled) {
      log.trace(new String(header ++ data))
    }

    out.write(header)
    out.write(data)
    out.flush()
  }

  protected def readString(input: InputStream): String = {
    def fillArray(a: Array[Byte]): Unit = {
      var n = 0
      while (n < a.length) {
        val read = input.read(a, n, a.length - n)
        if (read != -1) n += read
        else throw new EOFException("End of file reached in socket reader.")
      }
    }

    val headerBuf = new Array[Byte](6)

    fillArray(headerBuf)
    val msgLen = Integer.valueOf(new String(headerBuf, "UTF-8"), 16).intValue()
    if (msgLen == 0)
      throw new IllegalStateException("Empty message read from socket!")

    val buf: Array[Byte] = new Array[Byte](msgLen)
    fillArray(buf)

    val request = new String(buf, "UTF-8")
    if (log.isTraceEnabled) {
      log.trace(request)
    }
    request
  }

}
