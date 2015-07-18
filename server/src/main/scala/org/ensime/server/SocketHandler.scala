package org.ensime.server

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import akka.pattern.ask
import java.io._
import java.net.Socket
import org.ensime.api._
import org.ensime.core._
import org.ensime.server.protocol.swank._
import pimpathon.java.io._
import scala.util._
import scala.util.Properties._
import scala.util.control.NonFatal

/**
 * Handles a client connection to a Socket.
 */
class SocketHandler(
    protocol: Protocol,
    socket: Socket,
    broadcaster: ActorRef,
    project: ActorRef,
    docs: ActorRef,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging {

  private var handler: ActorRef = _

  private var in: InputStream = _
  private var out: OutputStream = _
  private var loop: Thread = _

  override def preStart(): Unit = {
    handler = context.actorOf(ConnectionHandler(project, broadcaster, docs, self), "handler")

    in = socket.getInputStream.buffered
    out = socket.getOutputStream.buffered

    loop = new Thread {
      var finished = false
      override def run(): Unit = while (!finished && !socket.isClosed) {
        try {
          handler ! protocol.read(in)
        } catch {
          case SwankRPCFormatException(msg, callId, cause) =>
            // specialist SWANK support
            self ! RpcResponseEnvelope(Some(callId), EnsimeServerError(msg))

          case e: IOException =>
            log.info("IOException seen - stopping reader")
            context.stop(self)
            finished = true
          case NonFatal(e) =>
            log.error(e, "Error in socket reader: ")
        }
      }
    }
    loop.setName("ENSIME Protocol Loop")
    loop.start()
  }

  override def postStop(): Unit = {
    log.info("Closing socket")
    Try(socket.close())
    Try(in.close())
    Try(out.close())
    Try(loop.interrupt())
  }

  def receive: Receive = {
    case outgoing: RpcResponseEnvelope =>
      try protocol.write(outgoing, out)
      catch {
        case NonFatal(t) =>
          log.error(t, s"Problem serialising $outgoing")
          protocol.write(
            RpcResponseEnvelope(
              outgoing.callId,
              EnsimeServerError(s"Server error: ${t.getMessage}")
            ),
            out
          )
      }
  }

}
object SocketHandler {
  def apply(
    protocol: Protocol,
    socket: Socket,
    broadcaster: ActorRef,
    project: ActorRef,
    docs: ActorRef
  )(implicit config: EnsimeConfig): Props =
    Props(classOf[SocketHandler], protocol, socket, broadcaster, project, docs, config)
}
