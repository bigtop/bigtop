package bigtop
package problem

import bigtop.json.JsonFormatters._
import blueeyes.core.http.HttpStatusCodes._
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

  object Authentication {
    def apply(credentials: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "authentication",
        message = "The user could not be authenticated.",
        cause   = cause,
        data    = ("credentials" -> credentials)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authentication")
        credentials <- in.data.get[String]("credentials").toOption
      } yield credentials
  }

  object Authorization {
    def apply(credentials: String, operation: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "authentication",
        message = "The user could not be authenticated.",
        cause   = cause,
        data    = ("credentials" -> credentials) ~ ("operation" -> operation)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("authentication")
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

  object Missing {
    def apply(field: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "missing",
        message = "Some required data was missing.",
        cause   = cause,
        data    = ("field" -> field)
      )

    def unapply(in: Problem) =
      for {
        _        <- in.checkId("missing")
        field    <- in.data.get[String]("field").toOption
      } yield field
  }

  object Malformed {
    def apply(field: String, description: String, cause: Option[Throwable] = None) =
      Problem(
        id      = "malformed",
        message = "Some required data was not in the expected format.",
        cause   = cause,
        data    = ("field" -> field) ~ ("description" -> description)
      )

    def unapply(in: Problem) =
      for {
        _           <- in.checkId("malformed")
        field       <- in.data.get[String]("field").toOption
        description <- in.data.get[String]("description").toOption
      } yield (field, description)
  }

  object MalformedRequest {
    def apply(cause: Option[Throwable] = None) =
      Problem(
        id      = "malformedRequest",
        message = "A request was incorrectly formatted.",
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

}
