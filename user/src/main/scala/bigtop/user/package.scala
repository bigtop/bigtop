package bigtop

import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import blueeyes.json.JsonAST._
import blueeyes.core.http.HttpRequest

package object user {

  type SecurityPredicate[A, U <: User] = (HttpRequest[A], Option[U]) => FutureValidation[Problem,Option[U]]

  trait UserTypes[U <: User] {

    type JsonValidation    = FutureValidation[Problem, JValue]
    type SessionValidation = FutureValidation[Problem, Session[U]]
    type UserValidation    = FutureValidation[Problem, U]
    type UnitValidation    = FutureValidation[Problem, Unit]

  }

}
