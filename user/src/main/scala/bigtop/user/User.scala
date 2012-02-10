package bigtop
package user


import bigtop.concurrent._
import bigtop.concurrent.FutureImplicits._
import bigtop.problem.Problem
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz.{NonEmptyList, ValidationNEL}

trait User {

  def username: String

  def isPasswordOk(passwd: String): Boolean

}

trait UserTypes[U <: User] {

  type JsonValidation = FutureValidation[Problem[String], JValue]
  type UserValidation = FutureValidation[Problem[String], U]
  type UnitValidation = FutureValidation[Problem[String], Unit]

}
