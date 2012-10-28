package bigtop
package problem

import blueeyes.core.http.HttpStatusCodes._

// Predefined problems

object Problems extends ProblemImplicits {
  import Problem._

  object Server {
    val empty: Problem =
      ServerProblem(Nil, Nil, InternalServerError)

    val databaseError: Problem =
      ServerProblem("databaseError")

    def typeError(msg: String, expected: String, received: String) =
      ServerProblem("typeError", "message" -> msg, "expected" -> expected, "received" -> received)

    def missing(field: String): Problem =
      ServerProblem("missing", "field" -> field)

    def malformed(field: String, description: String): Problem =
      ServerProblem("malformed", "field" -> field, "description" -> description)

    val noResponse: Problem =
      ServerProblem("noResponse")

    val malformedResponse: Problem =
      ServerProblem("malformedResponse")

    def timeout(msg: String) =
      ServerProblem("timeout", "message" -> msg)

    def unknown(msg: String) =
      ServerProblem("unknown", "message" -> msg)
  }

  object Client {
    val empty: Problem =
      ClientProblem(Nil, Nil, BadRequest)

    /** The client has no session. */
    val noSession: Problem =
      ClientProblem("noSession")

    /** The client was not authorized to perform an operation. */
    def notAuthorized(username: String, operation: String, args: (String, String)*): Problem =
      ClientProblem("unauthorized", Seq("username" -> username, "operation" -> operation) ++ args : _*)

    /** Some server functionality is not yet implemented. */
    def notImplemented(what: String): Problem =
      ClientProblem("notImplemented", "what" -> what)

    /** A record was not found on the server. */
    def notFound(item: String): Problem =
      ClientProblem("notFound", "item" -> item)

    /** A record already exists on the server. */
    def exists(item: String): Problem =
      ClientProblem("exists", "item" -> item)

    /** The client supplied a bodiless request. */
    val emptyRequest: Problem =
      ClientProblem("emptyRequest")

    /** The client supplied bad JSON. */
    def malformedRequest: Problem =
      ClientProblem("malformedRequest")

    def malformedRequest(description: String): Problem =
        ClientProblem("malformedRequest", "description" -> description)

    /** The client supplied a request with a missing argument. */
    def missing(field: String): Problem =
      ClientProblem("missing", "field" -> field)

    /** The client supplied a request with a malformed argument. */
    def malformed(field: String, description: String): Problem =
      ClientProblem("malformed", "field" -> field, "description" -> description)

    /** The login failed. We're not going to tell you too much about why because we don't want you to know if the username or password were incorrect */
    def loginUsernameIncorrect: Problem =
      ClientProblem("loginIncorrect")

    /** The login failed. We're not going to tell you too much about why because we don't want you to know if the username or password were incorrect */
    def loginPasswordIncorrect: Problem =
      ClientProblem("loginIncorrect")

    def customProblem(messageType: String, args: (String, String) *): Problem =
      ClientProblem(messageType, args : _*)
  }

}
