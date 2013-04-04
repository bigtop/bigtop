package bigtop
package problem

import bigtop.json._
import bigtop.json.JsonFormatters._
import blueeyes.core.http.HttpStatusCodes
import blueeyes.json.JsonDSL._

// Predefined problems

object Problems extends Problems

trait Problems {
  // Generic extractor:
  object ProblemWithId {
    def unapply(in: Problem) =
      Some(in.problemType)
  }

  object ClientValidation {
    def apply(
      errors: JsonErrors,
      message: String = "Some required data was in an incorrect format.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) = Problem(
      problemType = "validation",
      message     = message,
      cause       = cause,
      logMessage  = logMessage,
      status      = HttpStatusCodes.BadRequest,
      jsonErrors  = errors
    )

    def unapply(in: Problem) =
      for {
        _ <- in.checkType("validation")
      } yield in.jsonErrors
  }

  object ServerValidation {
    def apply(
      errors: JsonErrors,
      logMessage: Option[String] = None,
      message: String = "An unknown error occurred.",
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
      cause: Option[Throwable] = None
    ) = Problem(
      problemType = "authentication",
      message     = message,
      cause       = cause,
      status      = HttpStatusCodes.Forbidden,
      data        = ("credentials" -> credentials)
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
      cause: Option[Throwable] = None
    ) = Problem(
      problemType = "authorization",
      message     = "The user was not authorized to perform that action.",
      cause       = cause,
      status      = HttpStatusCodes.Forbidden,
      data        = ("credentials" -> credentials) ~ ("operation" -> operation)
    )

    def unapply(in: Problem) =
      for {
        _           <- in.checkType("authorization")
        credentials <- in.data.get[String]("credentials").toOption
        operation   <- in.data.get[String]("operation").toOption
      } yield (credentials, operation)
  }

  object NotFound {
    def apply(item: String, cause: Option[Throwable] = None) =
      Problem(
        problemType = "notFound",
        message     = "Some required data could not be found.",
        cause       = cause,
        status      = HttpStatusCodes.NotFound,
        data        = ("item" -> item)
      )

    def unapply(in: Problem) =
      for {
        _      <- in.checkType("notFound")
        item   <- in.data.get[String]("item").toOption
      } yield item
  }

  object Exists {
    def apply(item: String, cause: Option[Throwable] = None) =
      Problem(
        problemType = "exists",
        message     = "Some data already exists.",
        cause       = cause,
        data        = ("item" -> item)
      )

    def unapply(in: Problem) =
      for {
        _      <- in.checkType("exists")
        item   <- in.data.get[String]("item").toOption
      } yield item
  }

  object MalformedRequest {
    def apply(
      message: String = "The request was incorrectly formatted.",
      cause: Option[Throwable] = None
    ) =
      Problem(
        problemType = "malformedRequest",
        message     = message,
        cause       = cause
      )

    def unapply(in: Problem) =
      in.checkType("malformedRequest").isDefined
  }

  object EmptyRequest {
    def apply(cause: Option[Throwable] = None) =
      Problem(
        problemType = "emptyRequest",
        message     = "A request was empty.",
        cause       = cause
      )

    def unapply(in: Problem) =
      in.checkType("emptyRequest").isDefined
  }

  object Unknown {
    def apply(
      message: String = "An unknown error occurred.",
      cause: Option[Throwable] = None,
      logMessage: Option[String] = None
    ) = Problem(
      problemType = "unknown",
      message     = message,
      logMessage  = logMessage,
      cause       = cause
    )

    def unapply(in: Problem) =
      in.checkType("unknown").isDefined
  }

}
