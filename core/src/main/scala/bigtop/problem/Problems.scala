package bigtop
package problem

// Predefined problems

object Problems {

  object Request {
    val NoContent = BadRequest("The request did not contain any content")
    val NoUser = BadRequest("The request did not specify a user")
    val NoPassword = BadRequest("The request did not specify a password")
  }

}
