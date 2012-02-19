package bigtop
package user

import blueeyes.core.http.HttpRequest
import bigtop.problem.Problem
import bigtop.concurrent.FutureValidation
import scalaz.Validation

trait Authenticated[U <: User] {

  /** Return the user this request is authenticated as, if it is authenticated as a user. It's not an error to not be authenticated, so the result is an option not a Validation. */
  def authenticated[A](request: HttpRequest[A]): Option[U]

}

trait Authorized[U <: User] extends Authenticated[U] {

  def authorized[A,B](request: HttpRequest[A], condition: Option[U] => Validation[Problem,Option[U]])(f: Option[U] => B) = {
    val user = authenticated(request)
     for {
       u <- condition(user)
     } yield f(u)
  }

}
