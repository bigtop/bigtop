package bigtop

import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpRequest

package object user {

  type SecurityPredicate[A, U <: User] = (HttpRequest[A], Option[U]) => FutureValidation[Option[U]]

  trait UserTypes[U <: User] {

    type JsonValidation    = FutureValidation[JValue]
    type SessionValidation = FutureValidation[Session[U]]
    type UserValidation    = FutureValidation[U]
    type UnitValidation    = FutureValidation[Unit]

  }

}
