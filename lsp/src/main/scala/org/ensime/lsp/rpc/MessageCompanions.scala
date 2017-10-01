package org.ensime.lsp.rpc

import spray.json.{JsObject, JsonFormat}

import scala.language.existentials
import messages._
import messages.JsonRpcMessages._

import scala.util.{Failure, Success, Try}

trait Command[A] {
  val format: JsonFormat[A] = implicitly[JsonFormat[A]]
  val method: String
}

trait CommandCompanion[A] {

  protected[this] val commands: Seq[Command[_ <: A]]

  def read(jsonRpcRequestMessage: JsonRpcRequestMessage): Either[String, _ <: A] =
    commands.find(_.method == jsonRpcRequestMessage.method) match {
      case None => Left(s"unknown method ${jsonRpcRequestMessage.method}")
      case Some(command) =>
        jsonRpcRequestMessage.params match {
          case None       => Left("command parameters must be given")
          case Some(Right(arr)) => Left("command parameters must be named")
          case Some(Left(obj)) =>
            Try(command.format.read(obj)) match {
              // We do this just to reset the path in the success case.
              case Failure(invalid)    => Left(invalid.toString)
              case Success(valid) => Right(valid)
            }
        }
    }

  def write[B <: A](obj: B, id: CorrelationId)
                   (implicit command: Command[B]): JsonRpcRequestMessage = {
    val jsObj = command.format.write(obj) match {
      case o: JsObject => o
      case _ => sys.error(s"Wrong format for command $obj. Should be a json object.")
    }

    JsonRpcRequestMessage(
      command.method,
      Some(Left(jsObj)),
      id
    )
  }
}

trait ResponseCompanion[A] {

  def read[B <: A](jsonRpcResponseSuccessMessage: JsonRpcResponseSuccessMessage)
                  (implicit format: JsonFormat[B]): Either[String, B] =
    Try(format.read(jsonRpcResponseSuccessMessage.result)) match {
      // We do this just to reset the path in the success case.
      case Failure(invalid)    => Left(invalid.toString)
      case Success(valid) => Right(valid)
    }

  def write[B <: A](obj: B, id: CorrelationId)
                   (implicit format: JsonFormat[B]): JsonRpcResponseSuccessMessage =
    JsonRpcResponseSuccessMessage(
      format.write(obj),
      id
    )
}

trait Notification[A] {
  val format: JsonFormat[A] = implicitly[JsonFormat[A]]
  val method: String
}

trait NotificationCompanion[A] {

  protected[this] val notifications: Seq[Notification[_ <: A]]

  def read(jsonRpcNotificationMessage: JsonRpcNotificationMessage): Either[String, _ <: A] =
    notifications.find(_.method == jsonRpcNotificationMessage.method) match {
      case None => Left(s"unknown method ${jsonRpcNotificationMessage.method}")
      case Some(command) =>
        jsonRpcNotificationMessage.params match {
          case None       => Left("command parameters must be given")
          case Some(Right(arr)) => Left("command parameters must be named")
          case Some(Left(obj)) =>
            Try(command.format.read(obj)) match {
              // We do this just to reset the path in the success case.
              case Failure(invalid)    => Left(invalid.toString)
              case Success(valid) => Right(valid)
            }
        }
    }

  def write[B <: A](obj: B)
                   (implicit notification: Notification[B]): JsonRpcNotificationMessage = {
    val jsObj = notification.format.write(obj) match {
      case o: JsObject => o
      case _ => sys.error(s"Wrong format for command $obj. Should be a json object.")
    }

    JsonRpcNotificationMessage(
      notification.method,
      Some(Left(jsObj))
    )
  }
}