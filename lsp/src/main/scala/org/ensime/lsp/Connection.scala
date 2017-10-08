package org.ensime.lsp

import java.io.{ InputStream, OutputStream }
import java.util.concurrent.Executors

import akka.event.slf4j.SLF4JLogging
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.companions._
import org.ensime.lsp.api.types._
import org.ensime.lsp.rpc.RpcFormats._
import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.api.companions.Notifications._
import org.ensime.lsp.rpc.messages.JsonRpcMessages._
import org.ensime.lsp.rpc.messages.{
  JsonRpcResponseErrorMessage => RpcError,
  JsonRpcResponseErrorMessages => RpcErrors,
  _
}
import spray.json._

import scala.concurrent.ExecutionContext
import scala.util.{ Failure, Success, Try }

/**
 * A connection that reads and writes Language Server Protocol messages.
 *
 * @note Commands are executed asynchronously via a thread pool
 * @note Notifications are executed synchronously on the calling thread
 * @note The command handler returns Any because sometimes response objects can't be part
 *       of a sealed hierarchy. For instance, goto definition returns a {{{Seq[Location]}}}
 *       and that can't subclass anything other than Any
 */
class Connection(inStream: InputStream,
                 outStream: OutputStream,
                 notificationHandlers: Seq[Notification => Unit],
                 commandHandler: (
                   String,
                   ServerCommand,
                   CorrelationId
                 ) => Option[JsonRpcResponseSuccessMessage])
    extends SLF4JLogging {
  private val msgReader = new MessageReader(inStream)
  private val msgWriter = new MessageWriter(outStream)

  // 4 threads should be enough for everyone
  implicit private val commandExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  def notifySubscribers(n: Notification): Unit =
    notificationHandlers.foreach(
      f =>
        Try(f(n)).recover {
          case e => log.error("failed notification handler", e)
      }
    )

  def sendNotification[N <: Notification: RpcNotification](
    notification: N
  ): Unit = {
    val json = Notification.write(notification)
    msgWriter.write(json)
  }

  /**
   * A notification sent to the client to show a message.
   *
   * @param tpe One of MessageType values
   * @param message The message to display in the client
   */
  def showMessage(tpe: Int, message: String): Unit =
    sendNotification(ShowMessageParams(tpe, message))

  /**
   * The log message notification is sent from the server to the client to ask
   * the client to log a particular message.
   *
   * @param tpe One of MessageType values
   * @param message The message to display in the client
   */
  def logMessage(tpe: Int, message: String): Unit =
    sendNotification(LogMessageParams(tpe, message))

  /**
   * Publish compilation errors for the given file.
   */
  def publishDiagnostics(uri: String, diagnostics: Seq[Diagnostic]): Unit =
    sendNotification(PublishDiagnostics(uri, diagnostics))

  def handleMessage(): Boolean =
    msgReader.nextPayload() match {
      case None => false
      case Some(jsonString) =>
        readJsonRpcMessage(jsonString) match {
          case Left(e) =>
            msgWriter.write(e)

          case Right(message) =>
            message match {
              case notification: JsonRpcNotificationMessage =>
                Notification
                  .read(notification)
                  .fold(
                    {
                      case UnknownMethod =>
                        log.error(
                          s"No notification type exists with method=${notification.method}"
                        )
                      case e =>
                        log.error(
                          s"Invalid Notification: $e - Message: $message"
                        )
                    },
                    notifySubscribers
                  )
              case request: JsonRpcRequestMessage =>
                unpackRequest(request) match {
                  case Left(e) =>
                    msgWriter.write(e)
                  case Right(command) =>
                    commandHandler(request.method, command, request.id)
                      .foreach(msgWriter.write(_))
                }
              case response: JsonRpcResponseMessage =>
                log.info(s"Received response: $response")
              case m =>
                log.error(s"Received unknown message: $m")
            }
          case m => log.error(s"Received unknown message: $m")
        }
        true
    }

  def start(): Unit =
    while (handleMessage()) {}

  private def readJsonRpcMessage(
    jsonString: String
  ): Either[RpcError, JsonRpcMessage] = {
    log.debug(s"Received $jsonString")
    Try(JsonParser(jsonString)) match {
      case Failure(e) =>
        Left(RpcErrors.parseError(e, CorrelationId()))

      case Success(json) =>
        Try(JsonRpcMessageFormat.read(json)).fold(
          e => Left(RpcErrors.invalidRequest(e, CorrelationId())),
          Right(_)
        )
    }
  }

  private def readCommand(jsonString: String) // FIXME it's unused...
    : (Option[CorrelationId], Either[RpcError, ServerCommand]) =
    Try(JsonParser(jsonString)) match {
      case Failure(e) =>
        None -> Left(RpcErrors.parseError(e, CorrelationId()))
      case Success(json) =>
        Try(JsonRpcRequestMessageFormat.read(json)).fold(
          e => None -> Left(RpcErrors.invalidRequest(e, CorrelationId())),
          messsage =>
            Some(messsage.id) ->
              ServerCommand
                .read(messsage)
                .fold(
                  {
                    case UnknownMethod =>
                      Left(
                        RpcErrors.methodNotFound(messsage.method, messsage.id)
                      )
                    case e =>
                      Left(RpcErrors.invalidParams(e.describe, messsage.id))
                  },
                  command => Right(command)
              )
        )
    }

  private def unpackRequest(
    request: JsonRpcRequestMessage
  ): Either[RpcError, ServerCommand] =
    ServerCommand
      .read(request)
      .fold(
        {
          case UnknownMethod =>
            Left(RpcErrors.methodNotFound(request.method, request.id))
          case e =>
            Left(RpcErrors.invalidParams(e.describe, request.id))
        },
        command => Right(command)
      )

}
