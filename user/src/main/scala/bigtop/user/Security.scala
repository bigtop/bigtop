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
  def effectiveUser[A](request: HttpRequest[A]): FutureValidation[Option[U]]

  def realUser[A](request: HttpRequest[A]): FutureValidation[Option[U]]

  /** An alias for authenticate */
  def optional[A](request: HttpRequest[A]) = effectiveUser[A](request)

  /** Return the user this request is authenticated as, returning a Problem if there is no user. A convenience function */
  def mandatory[A](request: HttpRequest[A], operation: String): FutureValidation[U] =
    effectiveUser(request).flatMap {
      user => user.toSuccess(Client.notAuthorized("unknown", operation))
    }

  /** Return the session, if one exists, this request is authenticated as */
  def session[A](request: HttpRequest[A]): FutureValidation[Option[Session[U]]]

  def mandatorySession[A](request: HttpRequest[A], operation: String): FutureValidation[Session[U]] =
    session(request).flatMap {
      sess => sess.toSuccess(Client.notAuthorized("unknown", operation))
    }

}

trait Authorizer[U <: User] extends Authenticator[U] with FutureImplicits {

  def authorize[A,B](request: HttpRequest[A], condition: SecurityCheck[A,U]) = {
    for {
      user1 <- effectiveUser(request)
      user2 <- condition(request, user1).fv
    } yield user2
  }

}
