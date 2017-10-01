package org.ensime.lsp.rpc

import spray.json._
import shapeless._
import org.ensime.lsp.rpc.messages._

import scala.collection.immutable.Seq
import scala.util.Try

private object RpcConversions extends DefaultJsonProtocol with FamilyFormats {

  implicit val JsonRpcRequestMessageFormat: RootJsonFormat[JsonRpcRequestMessage] = cachedImplicit
  implicit val JsonRpcNotificationMessageFormat: RootJsonFormat[JsonRpcNotificationMessage] = cachedImplicit
  implicit val JsonRpcResponseSuccessMessageFormat: RootJsonFormat[JsonRpcResponseSuccessMessage] = cachedImplicit
  implicit val JsonRpcResponseErrorMessageFormat: RootJsonFormat[JsonRpcResponseErrorMessage] = cachedImplicit

  implicit object JsonRpcResponseMessageFormat extends RootJsonFormat[JsonRpcResponseMessage] {
    def read(j: JsValue): JsonRpcResponseMessage = j match {
      case JsObject(fields) =>
        if (fields.contains("error")) {
          j.convertTo[JsonRpcResponseErrorMessage]
        } else {
          j.convertTo[JsonRpcResponseSuccessMessage]
        }
      case _ => deserError("Response message should be an object")
    }

    def write(obj: JsonRpcResponseMessage): JsValue = obj match {
      case r: JsonRpcResponseErrorMessage => r.toJson
      case e: JsonRpcResponseErrorMessage => e.toJson
    }
  }

  implicit object JsonRpcRequestOrNotificationMessageFormat extends RootJsonFormat[JsonRpcRequestOrNotificationMessage] {
    def read(j: JsValue): JsonRpcRequestOrNotificationMessage = j match {
      case JsObject(fields) =>
        if (fields.contains("id")) {
          j.convertTo[JsonRpcRequestMessage]
        } else {
          j.convertTo[JsonRpcNotificationMessage]
        }
      case _ => deserError("Response message should be an object")
    }

    def write(obj: JsonRpcRequestOrNotificationMessage): JsValue = obj match {
      case r: JsonRpcRequestMessage => r.toJson
      case n: JsonRpcNotificationMessage => n.toJson
    }
  }

  implicit object JsonRpcRequestMessageBatchFormat extends RootJsonFormat[JsonRpcRequestMessageBatch] {
    def read(j: JsValue): JsonRpcRequestMessageBatch =
      JsonRpcRequestMessageBatch(j.convertTo[Seq[JsonRpcRequestOrNotificationMessage]])

    def write(obj: JsonRpcRequestMessageBatch): JsValue =
      obj.messages.toJson
  }

  implicit object JsonRpcResponseMessageBatchFormat extends RootJsonFormat[JsonRpcResponseMessageBatch] {
    def read(j: JsValue): JsonRpcResponseMessageBatch =
      JsonRpcResponseMessageBatch(j.convertTo[Seq[JsonRpcResponseMessage]])

    def write(obj: JsonRpcResponseMessageBatch): JsValue =
      obj.messages.toJson
  }

  implicit object JsonRpcMessageFormat extends RootJsonFormat[JsonRpcMessage] {
    // we should not introduce new fields to rpc jsons
    def read(j: JsValue): JsonRpcMessage = {
      val tryAll = Try(j.convertTo[JsonRpcRequestMessage]) orElse
        Try(j.convertTo[JsonRpcNotificationMessage]) orElse
        Try(j.convertTo[JsonRpcResponseSuccessMessage]) orElse
        Try(j.convertTo[JsonRpcResponseErrorMessage]) orElse
        Try(j.convertTo[JsonRpcRequestMessageBatch]) orElse
        Try(j.convertTo[JsonRpcResponseMessageBatch])

      tryAll.fold(
        _ => deserError("Error during JsonRpcMessage parsing"),
        x => x)
    }

    def write(obj: JsonRpcMessage): JsValue = obj match {
      case req: JsonRpcRequestMessage => req.toJson
      case n: JsonRpcNotificationMessage => n.toJson
      case suc: JsonRpcResponseSuccessMessage => suc.toJson
      case e: JsonRpcResponseErrorMessage => e.toJson
      case breq: JsonRpcRequestMessageBatch => breq.toJson
      case bres: JsonRpcResponseMessageBatch => bres.toJson
    }
  }

}

object RpcFormats {
  implicit val JsonRpcMessageFormat: RootJsonFormat[JsonRpcMessage] = RpcConversions.JsonRpcMessageFormat
}
