package bigtop
package problem

// Predefined problems

object Problems {
  import Problem._

  object Client {
    /** The client has no session. */
    val noSession =
      ClientProblem + Message("no-session")

    /** The client was not authorized to perform an operation. */
    def notAuthorized(username: String, operation: String) =
      ClientProblem + Message("unauthorized", Seq("username" -> username, "operation" -> operation))

    /** Some server functionality is not yet implemented. */
    def notImplemented(what: String) =
      ClientProblem + Message("not-implemented", Seq("what" -> what))

    /** A record was not found on the server. */
    def notFound(item: String) =
      ClientProblem + Message("not-found", Seq("item" -> item))

    /** A record already exists on the server. */
    def exists(item: String) =
      ClientProblem + Message("exists", Seq("item" -> item))

    /** The client supplied a bodiless request. */
    val emptyRequest =
      ClientProblem + Message("empty-request")

    /** The client supplied a request with a missing argument. */
    def missingArgument(argument: String) =
      ClientProblem + Message("missing-argument", Seq("argument" -> argument))

    /** The client supplied a request with a malformed argument. */
    def malformedArgument(argument: String, description: String) =
      ClientProblem + Message("malformed-argument", Seq("argument" -> argument, "description" -> description))
  }

}
