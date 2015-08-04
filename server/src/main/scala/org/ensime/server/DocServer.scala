package org.ensime.server

import akka.actor.Props
import akka.event.LoggingReceive
import akka.event.slf4j.SLF4JLogging
import java.io.{ File, IOException }
import java.util.jar.JarFile

import org.ensime.api._
import org.ensime.core._

import akka.actor.{ Actor, ActorLogging }
import akka.io.IO
import com.google.common.io.{ ByteStreams, Files }

import org.ensime.config._
import org.ensime.server.protocol._
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

import scala.collection.mutable

class DocServer(
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging {
  import context.system

  var port: Int = _
  var resolver: DocResolver = _

  override def preStart(): Unit = {
    IO(Http) ! Http.Bind(self, interface = "localhost", port = 0)
  }

  def receive = startup

  def startup = LoggingReceive.withLabel("startup") {
    case _: Http.Connected =>
      sender ! Http.Register(self)

    case Http.Bound(addr) =>
      port = addr.getPort
      resolver = new DocResolver

      context.become(serving)
  }

  def serving = LoggingReceive.withLabel("serving") {
    case HttpRequest(GET, p @ Uri.Path(_), _, _, _) =>
      val response = resolver.getJarEntry(p.path.toString()).map { bytes =>
        HttpResponse(entity = HttpEntity(
          MediaTypes.forExtension(Files.getFileExtension(p.path.toString()))
            .getOrElse(MediaTypes.`text/html`), HttpData(bytes)
        ))
      }.getOrElse(HttpResponse(status = 404))
      sender ! response

    case DocUriReq(sig) =>
      resolver.resolve(sig) match {
        case Some(hit) if hit.startsWith("http") => sender() ! StringResponse(hit)
        case Some(hit) => sender() ! StringResponse(s"http://localhost:$port/$hit")
        case None => sender() ! FalseResponse
      }
  }

}
object DocServer {
  def apply()(
    implicit
    config: EnsimeConfig
  ): Props = Props(classOf[DocServer], config)
}
