package org.ensime.server

import concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout

import org.ensime.api._
import org.ensime.core._
import org.ensime.jerk._
import shapeless._

class WebServerImpl(
    project: ActorRef,
    broadcaster: ActorRef,
    docs: ActorRef
)(
    implicit
    val system: ActorSystem,
    val mat: Materializer,
    val timeout: Timeout
) extends WebServer {
  import system.dispatcher

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage] = {
    (project ? Canonised(in)).mapTo[EnsimeServerMessage].map(Canonised(_))
  }
  def websocketHandler(target: ActorRef): ActorRef = {
    system.actorOf(ConnectionHandler(project, broadcaster, docs, target))
  }
}
