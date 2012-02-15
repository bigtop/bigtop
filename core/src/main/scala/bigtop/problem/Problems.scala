package bigtop
package problem

// Predefined problems

object Problems {

  object Client {
    val NoContent     = ClientProblem + "The request did not contain any content"
    val NoUser        = ClientProblem + ("username" -> "The request did not specify a user")
    val NoPassword    = ClientProblem + ("password" -> "The request did not specify a password")
    val UserExists    = ClientProblem + ("username" -> "The specified user already exists")
    val NoSession     = ClientProblem + "No session!"
    val NotAuthorized = ClientProblem + "I'm sorry, Dave. I cannot do that."
  }

}
