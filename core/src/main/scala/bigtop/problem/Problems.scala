package bigtop
package problem

import blueeyes.core.http.HttpStatusCodes._

// Predefined problems

object Problems extends ProblemImplicits {
  import Problem._

  def loc: SourceLocation =
    SourceLocation.atDepth(2)

  object Server {
    def empty: Problem =
      ServerProblem(loc, Nil, Nil, InternalServerError)

    def databaseError: Problem =
      ServerProblem(loc, "databaseError")

    def typeError(msg: String, expected: String, received: String) =
      ServerProblem(loc, "typeError", "message" -> msg, "expected" -> expected, "received" -> received)

    def missing(field: String): Problem =
      ServerProblem(loc, "missing", "field" -> field)

    def malformed(field: String, description: String): Problem =
      ServerProblem(loc, "malformed", "field" -> field, "description" -> description)

    def noResponse: Problem =
      ServerProblem(loc, "noResponse")

    def malformedResponse: Problem =
      ServerProblem(loc, "malformedResponse")

    def timeout(msg: String) =
      ServerProblem(loc, "timeout", "message" -> msg)

    def unknown(msg: String) =
      ServerProblem(loc, "unknown", "message" -> msg)
  }

  object Client {
    def empty: Problem =
      ClientProblem(loc, Nil, Nil, BadRequest)

    /** The client has no session. */
    def noSession: Problem =
      ClientProblem(loc, "noSession")

    /** The client was not authorized to perform an operation. */
    def notAuthorized(username: String, operation: String, args: (String, String)*): Problem =
      ClientProblem(loc, "unauthorized", Seq("username" -> username, "operation" -> operation) ++ args : _*)

    /** Some server functionality is not yet implemented. */
    def notImplemented(what: String): Problem =
      ClientProblem(loc, "notImplemented", "what" -> what)

    /** A record was not found on the server. */
    def notFound(item: String): Problem =
      ClientProblem(loc, "notFound", "item" -> item)

    /** A record already exists on the server. */
    def exists(item: String): Problem =
      ClientProblem(loc, "exists", "item" -> item)

    /** The client supplied a bodiless request. */
    def emptyRequest: Problem =
      ClientProblem(loc, "emptyRequest")

    /** The client supplied bad JSON. */
    def malformedRequest: Problem =
      ClientProblem(loc, "malformedRequest")

    /** The client supplied a request with a missing argument. */
    def missing(field: String): Problem =
      ClientProblem(loc, "missing", "field" -> field)

    /** The client supplied a request with a malformed argument. */
    def malformed(field: String, description: String): Problem =
      ClientProblem(loc, "malformed", "field" -> field, "description" -> description)

    /** The login failed. We're not going to tell you too much about why because we don't want you to know if the username or password were incorrect */
    def loginUsernameIncorrect: Problem =
      ClientProblem(loc, "loginIncorrect")

    /** The login failed. We're not going to tell you too much about why because we don't want you to know if the username or password were incorrect */
    def loginPasswordIncorrect: Problem =
      ClientProblem(loc, "loginIncorrect")

    def customProblem(messageType: String, args: (String, String) *): Problem =
      ClientProblem(loc, messageType, args : _*)
  }

}
