package org.ensime.server

import akka.http.scaladsl.model.HttpEntity
import akka.util.ByteString
import com.google.common.io.Files
import java.io.File

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

  /**
   * @param filename of the javadoc archive
   * @param entry of the file within the archive
   * @return the contents of the entry in filename
   */
  def docJarContent(filename: String, entry: String): Option[ByteString]

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
    } ~ path("docs" / """[^/]+\.jar""".r / Rest) { (filename, entry) =>
      rejectEmptyResponse {
        complete {
          for {
            media <- MediaTypes.forExtension(Files.getFileExtension(entry))
            content <- docJarContent(filename, entry)
          } yield {
            HttpResponse(entity = HttpEntity(ContentType(media, None), content))
          }
        }
      }
    } ~ path("jerky") {
      get {
        jsonWebsocket[RpcRequestEnvelope, RpcResponseEnvelope](websocketHandler)
      }
    }
  }

}
