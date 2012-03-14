package bigtop
package user

import blueeyes.core.http.HttpRequest
import bigtop.problem._
import bigtop.problem.Problems._
import bigtop.concurrent._
import scalaz.Validation
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

trait Authenticator[U <: User] {

  /** Return the user this request is authenticated as, if it is authenticated as a user. It's not an error to not be authenticated, so the user is an option. We have a validation as other errors can occur. */
  def authenticate[A](request: HttpRequest[A]): FutureValidation[Problem,Option[U]]

  /** An alias for authenticate */
  def optional[A](request: HttpRequest[A]) = authenticate[A](request)

  /** Return the user this request is authenticated as, returning a Problem if there is no user. A convenience function */
  def mandatory[A](request: HttpRequest[A], operation: String): FutureValidation[Problem, U] =
    authenticate(request).flatMap {
      user => user.toSuccess(Client.notAuthorized("unknown", operation))
    }

}

trait Authorizer[U <: User] extends Authenticator[U] with FutureImplicits {

  def authorize[A,B](request: HttpRequest[A], condition: SecurityCheck[A,U])(f: Option[U] => FutureValidation[Problem, B]) = {
    for {
      user1    <- authenticate(request)
      user2    <- condition(request, user1).fv
      response <- f(user2)
    } yield response
  }

}
