package bigtop
package user

import blueeyes.core.http.HttpRequest
import bigtop.problem.Problem
import bigtop.concurrent._
import scalaz.Validation

trait Authenticator[U <: User] {

  /** Return the user this request is authenticated as, if it is authenticated as a user. It's not an error to not be authenticated, so the user is an option. We have a validation as other errors can occur. */
  def authenticate[A](request: HttpRequest[A]): FutureValidation[Problem,Option[U]]

}

trait Authorizer[U <: User] extends Authenticator[U] with FutureImplicits {

  def authorize[A,B](request: HttpRequest[A], condition: Option[U] => FutureValidation[Problem,Option[U]])(f: Option[U] => FutureValidation[Problem, B]) = {
    for {
      user1    <- authenticate(request)
      user2    <- condition(user1).fv
      response <- f(user2)
    } yield response
  }

}
