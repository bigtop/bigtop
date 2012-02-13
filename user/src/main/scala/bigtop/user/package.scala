package bigtop


import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import blueeyes.json.JsonAST._


package object user {
  type Error = (String, String)

  trait UserTypes[U <: User] {

    type JsonValidation = FutureValidation[Problem[String], JValue]
    type UserValidation = FutureValidation[Problem[String], U]
    type UnitValidation = FutureValidation[Problem[String], Unit]

  }

}
