package bigtop

import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import blueeyes.core.http.HttpRequest
import blueeyes.json.JsonAST._

package object user {

  type SecurityCheck[A, U <: User] = (HttpRequest[A], Option[U]) => FutureValidation[Problem,Option[U]]

  trait UserTypes[U <: User] {

    type JsonValidation    = FutureValidation[Problem, JValue]
    type SessionValidation = FutureValidation[Problem, Session[U]]
    type UserValidation    = FutureValidation[Problem, U]
    type UnitValidation    = FutureValidation[Problem, Unit]

  }

}
