package org.ensime.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import concurrent.Future

import akka.actor.ActorRef
import akka.event.slf4j.SLF4JLogging
import akka.http.scaladsl.testkit._
import akka.stream.ActorMaterializer
import akka.testkit._
import org.scalatest._
import akka.http.scaladsl.model.StatusCodes

import spray.json._

import org.ensime.api._
import org.ensime.core._

class WebServerSpec extends HttpFlatSpec with WebServer {

  import SprayJsonSupport._

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage] =
    Future.successful(SendBackgroundMessageEvent("hello"))

  val expected = """{"typehint":"SendBackgroundMessageEvent","detail":"hello","code":105}""".parseJson

  val probe = TestProbe()
  def websocketHandler(target: ActorRef): ActorRef = probe.ref

  "WebServer" should "respond to REST queries" in {
    Post("/rpc", """{"typehint":"ConnectionInfoReq"}""".parseJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[JsValue] shouldBe expected
    }
  }

  it should "error to bad REST queries" in {
    Get("/rpc") ~> route ~> check {
      status shouldBe StatusCodes.MethodNotAllowed
    }
  }

  it should "respond to WebSocket queries" ignore {
    // https://github.com/akka/akka/issues/17914
    fail("no test framework yet")
  }

}

/**
 * Equivalent for akka-http-testkit use (non-trivial ordering of mixins)
 * http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0/scala/http/routing-dsl/testkit.html
 */
abstract class HttpFlatSpec
    extends FlatSpecLike with SetupAndTearDownSystem
    with ScalatestRouteTest
    with TestKitBase with DefaultTimeout with ImplicitSender
    with Matchers with SLF4JLogging {
  def actorRefFactory = system
  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(timeout.duration.dilated)
  implicit val mat = ActorMaterializer()
}
