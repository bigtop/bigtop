package bigtop
package user


import bigtop.concurrent._
import bigtop.concurrent.FutureImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz.{NonEmptyList, ValidationNEL}

trait User {

  def username: String

  def isPasswordOk(passwd: String): Boolean

}

trait UserTypes[U <: User] {

  type JsonValidation = FutureValidation[NonEmptyList[Error], JValue]
  type UserValidation = FutureValidation[NonEmptyList[Error], U]
  type UnitValidation = FutureValidation[NonEmptyList[Error], Unit]

}

trait UserStore[U <: User] extends UserTypes[U] {

  def get(username: String): UserValidation

  def add(user: U): UserValidation

  def delete(username: String): UnitValidation

}
