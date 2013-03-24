package bigtop
package problem

import blueeyes.core.http.HttpStatusCodes._

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

  object Authentication {
    def apply(credentials: String) =
      Problem(
        id      = "authentication",
        message = "The user could not be authenticated.",
        data    = Map("credentials" -> credentials)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authentication")
        credentials <- in.data.get("credentials")
      } yield credentials
  }

  object Authorization {
    def apply(credentials: String, operation: String) =
      Problem(
        id      = "authentication",
        message = "The user could not be authenticated.",
        data    = Map("credentials" -> credentials, "operation" -> operation)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authentication")
        credentials <- in.data.get("credentials")
        operation   <- in.data.get("operation")
      } yield (credentials, operation)
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
  //       expected <- in.data.get("expected")
  //       received <- in.data.get("received")
  //     } yield (expected, received)
  // }

  object Missing {
    def apply(field: String) =
      Problem(
        id      = "missing",
        message = "Some required data was missing.",
        data    = Map("field" -> field)
      )

    def unapply(in: Problem) =
      for {
        _        <- in.checkId("missing")
        field    <- in.data.get("field")
      } yield field
  }

  object Malformed {
    def apply(field: String, description: String) =
      Problem(
        id      = "malformed",
        message = "Some required data was not in the expected format.",
        data    = Map("field" -> field, "description" -> description)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("malformed")
        field       <- in.data.get("field")
        description <- in.data.get("description")
      } yield (field, description)
  }

  object MalformedRequest {
    def apply() =
      Problem(
        id      = "malformedRequest",
        message = "A request was incorrectly formatted."
      )

    def unapply(in: Problem) =
      in.checkId("malformedRequest").isDefined
  }

  object EmptyRequest {
    def apply() =
      Problem(
        id      = "emptyRequest",
        message = "A request was empty."
      )

    def unapply(in: Problem) =
      in.checkId("emptyRequest").isDefined
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
