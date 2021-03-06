package bigtop

import bigtop.concurrent._
import bigtop.problem.Problem
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpRequest

package object user {

  type SecurityPredicate[A, U <: User] = (HttpRequest[A], Option[U]) => FutureValidation[Option[U]]

  trait UserTypes[U <: User] {
    type SessionValidation = FutureValidation[Session[U]]
    type UserValidation    = FutureValidation[U]
  }

}
