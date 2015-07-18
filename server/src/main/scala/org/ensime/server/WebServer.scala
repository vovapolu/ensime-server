package org.ensime.server

import concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import akka.stream._
import akka.stream.scaladsl._
import akka.http.scaladsl.server._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import org.ensime.api._
import org.ensime.core._
import org.ensime.jerk._

trait WebServer {
  implicit def system: ActorSystem
  implicit def timeout: Timeout
  implicit def mat: Materializer

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage]

  def websocketHandler(target: ActorRef): ActorRef

  import Directives._
  import SprayJsonSupport._
  import Route._

  import JerkFormats._
  import JerkEnvelopeFormats._
  import WebSocketBoilerplate._

  val route = seal {
    path("rpc") {
      post {
        entity(as[RpcRequest]) { request =>
          complete {
            restHandler(request)
          }
        }
      }
    } ~ path("docs") {
      // TODO: migrate the DocServer here, remove "docs" ActorRef
      ???
    } ~ path("jerky") {
      get {
        jsonWebsocket[RpcRequestEnvelope, RpcResponseEnvelope](websocketHandler)
      }
    }
  }

}
