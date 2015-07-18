package org.ensime.server

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.google.common.base.Charsets
import com.google.common.io.Files
import java.io._
import java.net.{ InetAddress, ServerSocket, Socket }
import java.util.concurrent.atomic.AtomicBoolean
import org.ensime.api._
import org.ensime.config._
import org.ensime.core._
import org.slf4j._
import org.slf4j.bridge.SLF4JBridgeHandler
import scala.concurrent.duration._
import scala.util._
import scala.util.Properties._
import akka.http.scaladsl.server.RouteResult.route2HandlerFlow

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  val log = LoggerFactory.getLogger(classOf[Server])

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set")
    )

    val ensimeFile = new File(ensimeFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")

    implicit val config = try {
      EnsimeConfigProtocol.parse(Files.toString(ensimeFile, Charsets.UTF_8))
    } catch {
      case e: Throwable =>
        log.error(s"There was a problem parsing $ensimeFile", e)
        throw e
    }

    val protocol: Protocol = propOrElse("ensime.protocol", "swank") match {
      case "swank" => new SwankProtocol
      case "jerk" => new JerkProtocol
      case other => throw new IllegalArgumentException(s"$other is not a valid ENSIME protocol")
    }

    new Server(protocol).start()
  }

}

/**
 * The Legacy Socket handler is a bit nasty --- it was originally
 * written before there were any decent Scala libraries for IO so we
 * end up doing Socket loops in Threads.
 *
 * It's crying out to be rewritten with akka.io.
 */
class Server(
    protocol: Protocol,
    interface: String = "127.0.0.1"
)(
    implicit
    config: EnsimeConfig
) extends SLF4JLogging {
  // the config file parsing will attempt to create directories that are expected
  require(config.cacheDir.isDirectory, s"${config.cacheDir} is not a valid cache directory")

  implicit private val system = ActorSystem("ENSIME")
  implicit private val mat = ActorMaterializer()
  implicit private val timeout = Timeout(10 seconds)

  val broadcaster = system.actorOf(Broadcaster(), "broadcaster")
  val project = system.actorOf(Project(broadcaster), "project")
  val docs = system.actorOf(Props(new DocServer(config)), "docs")

  val webserver = new WebServerImpl(project, broadcaster, docs)

  // async start the HTTP Server
  Http().bindAndHandle(webserver.route, interface, 0).onSuccess {
    case ServerBinding(addr) =>
      log.info(s"ENSIME HTTP on ${addr.getAddress}")
      writePort(config.cacheDir, addr.getPort, "http")
  }(system.dispatcher)

  // synchronously start the Socket Server
  private val listener = new ServerSocket(0, 0, InetAddress.getByName(interface))
  private val hasShutdownFlag = new AtomicBoolean
  private var loop: Thread = _

  log.info("Starting ENSIME TCP on " + interface + ":" + listener.getLocalPort)
  log.info(Environment.info)

  writePort(config.cacheDir, listener.getLocalPort, "port")

  def start(): Unit = {
    loop = new Thread {
      override def run(): Unit = {
        try {
          while (!hasShutdownFlag.get()) {
            val socket = listener.accept()
            system.actorOf(SocketHandler(protocol, socket, broadcaster, project, docs))
          }
        } catch {
          case e: Exception =>
            if (!hasShutdownFlag.get())
              log.error("ENSIME Server socket listener", e)
        } finally listener.close()
      }
    }
    loop.setName("ENSIME Connection Loop")
    loop.start()
  }

  // TODO: attach this to the appropriate KILL signal
  def shutdown(): Unit = {
    hasShutdownFlag.set(true)
    Try(loop.interrupt())

    log.info("Shutting down the ActorSystem")
    Try(system.shutdown())

    log.info("Closing Socket listener")
    Try(listener.close())

    log.info("Awaiting actor system termination")
    Try(system.awaitTermination())

    log.info("Shutdown complete")
  }

  private def writePort(cacheDir: File, port: Int, name: String): Unit = {
    val portfile = new File(cacheDir, name)
    if (!portfile.exists()) {
      log.info("creating portfile " + portfile)
      portfile.createNewFile()
    } else throw new IOException(
      "An ENSIME server might be open already for this project. " +
        "If you are sure this is not the case, please delete " +
        portfile.getAbsolutePath + " and try again"
    )

    portfile.deleteOnExit()
    val out = new PrintWriter(portfile)
    try out.println(port)
    finally out.close()
  }
}
