package bigtop

import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import blueeyes.json.JsonAST._

package object user {
  type Error = (String, String)

  trait UserTypes[U <: User] {

    type JsonValidation    = FutureValidation[Problem, JValue]
    type SessionValidation = FutureValidation[Problem, Session[U]]
    type UserValidation    = FutureValidation[Problem, U]
    type UnitValidation    = FutureValidation[Problem, Unit]

  }

}
