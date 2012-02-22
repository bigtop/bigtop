package bigtop
package user

import blueeyes.core.http.HttpRequest
import bigtop.problem.Problem
import bigtop.concurrent._
import scalaz.Validation

trait Authenticated[U <: User] {

  /** Return the user this request is authenticated as, if it is authenticated as a user. It's not an error to not be authenticated, so the user is an option. We have a validation as other errors can occur. */
  def authenticated[A](request: HttpRequest[A]): FutureValidation[Problem,Option[U]]

}

trait Authorized[U <: User] extends Authenticated[U] with FutureImplicits {

  def authorized[A,B](request: HttpRequest[A], condition: Option[U] => Validation[Problem,Option[U]])(f: Option[U] => FutureValidation[Problem, B]) = {
    for {
      user1    <- authenticated(request)
      user2    <- condition(user1).fv
      response <- f(user2)
    } yield response
  }

}
