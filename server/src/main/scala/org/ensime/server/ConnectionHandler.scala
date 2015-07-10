package org.ensime.server

import akka.actor._
import akka.event.LoggingReceive
import akka.event.slf4j.SLF4JLogging
import akka.pattern.ask
import org.ensime.api._
import org.ensime.core._
import scala.util.Properties._
import shapeless._

/**
 * Accepts RpcRequestEnvelope and responds with an RpcResponseEnvelope to target.
 * Also sends asynchronous RpcResponseEnvelopes to target.
 * Ensures that everything in and out is canonised.
 */
class ConnectionHandler(
    project: ActorRef,
    broadcaster: ActorRef,
    docs: ActorRef,
    target: ActorRef
) extends Actor with ActorLogging {

  override def preStart(): Unit = {
    broadcaster ! Broadcaster.Register
  }

  override def postStop(): Unit = {
    broadcaster ! Broadcaster.Unregister
  }

  def receive: Receive = receiveRpc orElse LoggingReceive { receiveEvents }

  def receiveRpc: Receive = {
    case req: RpcRequestEnvelope =>
      val handler = RequestHandler(Canonised(req), project, self, docs)
      context.actorOf(handler, s"${req.callId}")

    case outgoing: RpcResponseEnvelope =>
      target forward Canonised(outgoing)
  }

  def receiveEvents: Receive = {
    case outgoing: EnsimeEvent =>
      target forward RpcResponseEnvelope(None, Canonised(outgoing))
  }

}
object ConnectionHandler {
  def apply(
    project: ActorRef,
    broadcaster: ActorRef,
    docs: ActorRef,
    target: ActorRef
  ): Props = Props(classOf[ConnectionHandler], project, broadcaster, docs, target)
}
