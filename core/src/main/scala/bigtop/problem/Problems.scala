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
      Some(in.id)
  }

  // object Database {
  //   def apply() =
  //     Problem(
  //       id      = "databaseError",
  //       message = "There was an error connecting to the database."
  //     )

  //   def unapply(in: Problem) =
  //     in.checkId("databaseError").isDefined
  // }

  object ClientValidation {
    def apply(
      errors: JsonErrors,
      message: String = "Some required data was in an incorrect format.",
      logMessage: Option[String] = None,
      cause: Option[Throwable] = None
    ) = Problem(
      id         = "validation",
      message    = message,
      cause      = cause,
      logMessage = logMessage,
      status     = HttpStatusCodes.BadRequest,
      data       = ("errors" -> errors.toJson)
    )

    def unapply(in: Problem) =
      for {
        _      <- in.checkId("validation")
        errors <- in.data.get[JsonErrors]("errors").toOption
      } yield errors
  }

  object ServerValidation {
    def apply(
      errors: JsonErrors,
      logMessage: Option[String] = None,
      message: String = "An unknown error occurred.",
      cause: Option[Throwable] = None
    ) = Problem(
      id         = "unknown",
      message    = message,
      cause      = cause,
      logMessage = Some(logMessage.getOrElse("Error parsing JSON.") + "\n" + errors.toString),
      status     = HttpStatusCodes.InternalServerError
    )

    def unapply(in: Problem) =
      in.checkId("unknown").isDefined
  }

  object Authentication {
    def apply(
      credentials: String,
      message: String = "The user could not be authenticated.",
      cause: Option[Throwable] = None
    ) = Problem(
      id      = "authentication",
      message = message,
      cause   = cause,
      status  = HttpStatusCodes.Forbidden,
      data    = ("credentials" -> credentials)
    )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authentication")
        credentials <- in.data.get[String]("credentials").toOption
      } yield credentials
  }

  object Authorization {
    def apply(
      credentials: String,
      operation: String,
      cause: Option[Throwable] = None
    ) = Problem(
      id      = "authorization",
      message = "The user was not authorized to perform that action.",
      cause   = cause,
      status  = HttpStatusCodes.Forbidden,
      data    = ("credentials" -> credentials) ~ ("operation" -> operation)
    )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authorization")
        credentials <- in.data.get[String]("credentials").toOption
        operation   <- in.data.get[String]("operation").toOption
      } yield (credentials, operation)
  }

  object NotFound {
    def apply(item: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "notFound",
        message = "Some required data could not be found.",
        cause   = cause,
        status  = HttpStatusCodes.NotFound,
        data    = ("item" -> item)
      )

    def unapply(in: Problem) =
      for {
        _      <- in.checkId("notFound")
        item   <- in.data.get[String]("item").toOption
      } yield item
  }

  object Exists {
    def apply(item: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "exists",
        message = "Some data already exists.",
        cause   = cause,
        data    = ("item" -> item)
      )

    def unapply(in: Problem) =
      for {
        _      <- in.checkId("exists")
        item   <- in.data.get[String]("item").toOption
      } yield item
  }

  // object TypeError {
  //   def apply(expected: String, received: String) =
  //     Problem(
  //       id      = "typeError",
  //       message = "Some data was of the incorrect type.",
  //       data    = Map("expected" -> expected, "received" -> received)
  //     )

  //   def unapply(in: Problem) =
  //     for {
  //       _        <- in.checkId("typeError")
  //       expected <- in.data.get[String]("expected").toOption
  //       received <- in.data.get[String]("received").toOption
  //     } yield (expected, received)
  // }

  // object Missing {
  //   def apply(field: String, cause: Option[Throwable] = None) =
  //     Problem(
  //       id      = "missing",
  //       message = "Some required data was missing.",
  //       cause   = cause,
  //       data    = ("field" -> field)
  //     )

  //   def unapply(in: Problem) =
  //     for {
  //       _        <- in.checkId("missing")
  //       field    <- in.data.get[String]("field").toOption
  //     } yield field
  // }

  // object Malformed {
  //   def apply(field: String, description: String, cause: Option[Throwable] = None) =
  //     Problem(
  //       id      = "malformed",
  //       message = "Some required data was not in the expected format.",
  //       cause   = cause,
  //       data    = ("field" -> field) ~ ("description" -> description)
  //     )

  //   def unapply(in: Problem) =
  //     for {
  //       _           <- in.checkId("malformed")
  //       field       <- in.data.get[String]("field").toOption
  //       description <- in.data.get[String]("description").toOption
  //     } yield (field, description)
  // }

  object MalformedRequest {
    def apply(cause: Option[Throwable] = None) =
      Problem(
        id      = "malformedRequest",
        message = "The request was incorrectly formatted.",
        cause   = cause
      )

    def unapply(in: Problem) =
      in.checkId("malformedRequest").isDefined
  }

  object EmptyRequest {
    def apply(cause: Option[Throwable] = None) =
      Problem(
        id      = "emptyRequest",
        message = "A request was empty.",
        cause   = cause
      )

    def unapply(in: Problem) =
      in.checkId("emptyRequest").isDefined
  }

  object Unknown {
    def apply(
      message: String = "An unknown error occurred.",
      cause: Option[Throwable] = None,
      logMessage: Option[String] = None
    ) = Problem(
      id         = "unknown",
      message    = message,
      logMessage = logMessage,
      cause      = cause
    )

    def unapply(in: Problem) =
      in.checkId("unknown").isDefined
  }

  //   case class NoResponse(
  //     val message: String = "An upstream service returned no response.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ServerProblem

  //   case class MalformedResponse(
  //     val message: String = "An upstream service returned a response in an unexpected format.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ServerProblem

  //   case class Timeout(
  //     val message: String = "A timeout occurred.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ServerProblem

  //   case class Unknown(
  //     val message: String = "An unknown error occurred.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ServerProblem

  // object Client {
  //   case class NoSession(
  //     val message: String = "The user or client has no session.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem

  //   case class NotAuthorized(
  //     val username: String,
  //     val operation: String,
  //     val message: String = "The user or client is not authorized to perform a required action.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     override def data = ("username" -> username) ~ ("operation" -> operation)
  //   }

  //   case class NotImplemented(
  //     val what: String,
  //     val message: String = "A required service is currently unimplemented.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     override def data = ("what" -> what)
  //   }

  //   case class NotFound(
  //     val item: String,
  //     val message: String = "A required resource oculd not be found.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     override def data = ("item" -> item)
  //   }

  //   case class Exists(
  //     val item: String,
  //     val message: String = "A resource already exists.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     override def data = ("item" -> item)
  //   }

  //   case class EmptyRequest(
  //     val message: String = "The client sent an empty request.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem

  //   case class MalformedRequest(
  //     val description: String,
  //     val message: String = "The client sent an incorrectly formatted request.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     def data = ("description" -> description)
  //   }

  //   case class Missing(
  //     val field: String,
  //     val message: String = "The request was missing some required data.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     def data = ("field" -> field)
  //   }

  //   case class Malformed(
  //     val field: String,
  //     val description: String,
  //     val message: String = "The request contained some incorrectly formatted data.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem {
  //     def data = ("field" -> field) ~ ("description" -> description)
  //   }

  //   case class LoginIncorrect(
  //     val message: String = "The client did not have valid authentication credentials.",
  //     val logMessage: Option[String] = None,
  //     val cause: Option[Throwable] = None
  //   ) extends ClientProblem
  // }

}
