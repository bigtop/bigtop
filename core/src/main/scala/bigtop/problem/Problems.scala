package bigtop
package problem

// Predefined problems

object Problems extends ProblemImplicits {
  import Problem._

  object Server {
    val databaseError: Problem =
      ServerProblem("databaseError")
  }

  object Client {
    /** The client has no session. */
    val noSession: Problem =
      ClientProblem("noSession")

    /** The client was not authorized to perform an operation. */
    def notAuthorized(username: String, operation: String): Problem =
      ClientProblem("unauthorized", "username" -> username, "operation" -> operation)

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
    val malformedRequest: Problem =
      ClientProblem("malformedRequest")

    /** The client supplied a request with a missing argument. */
    def missingArgument(argument: String): Problem =
      ClientProblem("missingArgument", "argument" -> argument)

    /** The client supplied a request with a malformed argument. */
    def malformedArgument(argument: String, description: String): Problem =
      ClientProblem("malformedArgument", "argument" -> argument, "description" -> description)

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
