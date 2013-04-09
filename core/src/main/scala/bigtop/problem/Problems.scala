package bigtop
package problem

import bigtop.json._
import bigtop.json.JsonFormatters._
import blueeyes.core.http.{HttpStatusCode, HttpStatusCodes}
import blueeyes.json.JsonDSL._

// Predefined problems

object Problems extends Problems

trait Problems {
  object ClientValidation {
    def apply(
      jsonErrors: JsonErrors,
      message: String = "Some required data was in an incorrect format.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) = Problem(
      problemType = "validation",
      message     = message,
      cause       = cause,
      logMessage  = logMessage,
      status      = HttpStatusCodes.BadRequest,
      jsonErrors  = jsonErrors
    )

    def unapply(in: Problem) =
      for {
        _ <- in.checkType("validation")
      } yield in.jsonErrors
  }

  object ServerValidation {
    def apply(
      errors: JsonErrors,
      message: String = "An unknown error occurred.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) = Problem(
      problemType = "unknown",
      message     = message,
      cause       = cause,
      logMessage  = Some(logMessage.getOrElse("Error parsing JSON.") + "\n" + errors.toString),
      status      = HttpStatusCodes.InternalServerError
    )

    def unapply(in: Problem) =
      in.checkType("unknown").isDefined
  }

  object Authentication {
    def apply(
      credentials: String,
      message: String = "The user could not be authenticated.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      status: HttpStatusCode = HttpStatusCodes.Forbidden
    ) = Problem(
      problemType = "authentication",
      message     = message,
      logMessage  = logMessage,
      cause       = cause,
      status      = status,
      data        = JsonConfig("credentials" -> credentials)
    )

    def unapply(in: Problem) =
      for {
        _           <- in.checkType("authentication")
        credentials <- in.data.get[String]("credentials").toOption
      } yield credentials
  }

  object Authorization {
    def apply(
      credentials: String,
      operation: String,
      message: String = "The user was not authorized to perform that action.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      status: HttpStatusCode = HttpStatusCodes.Forbidden
    ) = Problem(
      problemType = "authorization",
      message     = message,
      logMessage  = logMessage,
      cause       = cause,
      status      = status,
      data        = JsonConfig(("credentials" -> credentials) ~ ("operation" -> operation))
    )

    def unapply(in: Problem) =
      for {
        _           <- in.checkType("authorization")
        credentials <- in.data.get[String]("credentials").toOption
        operation   <- in.data.get[String]("operation").toOption
      } yield (credentials, operation)
  }

  object NotFound {
    def apply(
      itemType: String,
      item: String,
      message: Option[String] = None,
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      status: HttpStatusCode = HttpStatusCodes.NotFound,
      data: JsonConfig = JsonConfig.Empty
    ) =
      Problem(
        problemType = "notFound",
        message     = message getOrElse ("That " + itemType + " could not be found."),
        logMessage  = logMessage,
        cause       = cause,
        status      = status,
        data        = data merge JsonConfig(("itemType" -> itemType) ~ ("item" -> item))
      )

    def unapply(in: Problem) =
      for {
        _        <- in.checkType("notFound")
        itemType <- in.data.get[String]("itemType").toOption
        item     <- in.data.get[String]("item").toOption
      } yield (itemType, item)
  }

  object Exists {
    def apply(
      itemType: String,
      item: String,
      message: Option[String] = None,
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      status: HttpStatusCode = HttpStatusCodes.BadRequest,
      data: JsonConfig = JsonConfig.Empty
    ) =
      Problem(
        problemType = "exists",
        message     = message getOrElse ("That " + itemType + " already exists."),
        logMessage  = logMessage,
        cause       = cause,
        status      = status,
        data        = data merge JsonConfig(("itemType" -> itemType) ~ ("item" -> item))
      )

    def unapply(in: Problem) =
      for {
        _        <- in.checkType("exists")
        itemType <- in.data.get[String]("itemType").toOption
        item     <- in.data.get[String]("item").toOption
      } yield (itemType, item)
  }

  object MalformedRequest {
    def apply(
      message: String = "The request was incorrectly formatted.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) =
      Problem(
        problemType = "malformedRequest",
        message     = message,
        logMessage  = logMessage,
        cause       = cause
      )

    def unapply(in: Problem) =
      in.checkType("malformedRequest").isDefined
  }

  object MalformedResponse {
    def apply(
      message: String = "The response was incorrectly formatted.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      status: HttpStatusCode = HttpStatusCodes.InternalServerError
    ) =
      Problem(
        problemType = "malformedResponse",
        message     = message,
        logMessage  = logMessage,
        cause       = cause,
        status      = status
      )

    def unapply(in: Problem) =
      in.checkType("malformedResponse").isDefined
  }

  object EmptyRequest {
    def apply(
      message: String = "A request was empty.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) =
      Problem(
        problemType = "emptyRequest",
        message     = message,
        logMessage  = logMessage,
        cause       = cause
      )

    def unapply(in: Problem) =
      in.checkType("emptyRequest").isDefined
  }

  object Unknown {
    def apply(
      message: String = "An unknown error occurred.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None,
      data: JsonConfig = JsonConfig()
    ) = Problem(
      problemType = "unknown",
      message     = message,
      logMessage  = logMessage,
      cause       = cause,
      data        = data
    )

    def unapply(in: Problem) =
      in.checkType("unknown").isDefined
  }

}
