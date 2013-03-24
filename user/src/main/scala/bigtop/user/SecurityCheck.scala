package bigtop
package user

import bigtop.concurrent.{FutureImplicits,FutureValidation}
import bigtop.problem._
import blueeyes.core.http.HttpRequest
import scalaz.syntax.validation._

case class SecurityCheck[A,U <: User](val predicate: SecurityPredicate[A,U]) extends FutureImplicits {

  def apply(request: HttpRequest[A], user: Option[U]): FutureValidation[Option[U]] =
    predicate(request, user)

  def or(other: SecurityCheck[A,U]): SecurityCheck[A,U] =
    SecurityCheck((request, user) => predicate(request, user).orElse(_ => other(request, user)))

}

object SecurityCheck extends FutureImplicits {

  def simpleCheck[A,U <: User](f: Option[U] => Boolean, operation: String): SecurityCheck[A,U] =
    SecurityCheck(
      (request: HttpRequest[A], user: Option[U]) =>
        if(f(user))
          user.success[Problem].fv
        else
          Problems.Authorization(user.map(_.username).getOrElse("unknown"), operation).fail.fv
    )

  def isAuthenticated[A,U <: User](operation: String): SecurityCheck[A,U] =
    simpleCheck((u: Option[U]) => u.isDefined, operation)

}
