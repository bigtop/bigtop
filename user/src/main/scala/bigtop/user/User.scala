package bigtop
package user

import bigtop.concurrent._
import bigtop.concurrent.Implicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import Scalaz._

trait User {

  def username: String
  
  def isPasswordOk(passwd: String): Boolean
  
}

trait UserTypes[U <: User] {

  type JsonValidation = FutureValidation[NonEmptyList[Error], JValue]
  type UserValidation = FutureValidation[NonEmptyList[Error], U]
  type UnitValidation = FutureValidation[NonEmptyList[Error], Unit]

}

trait Reader[E,T] {
  def read(j: JValue): ValidationNEL[E,T]
}

trait Writer[T] {
  def write(t: T): JValue
}

trait Updater[E,T] {
  def update(t: T, j: JValue): ValidationNEL[E,T]
}

trait Format[E,T] extends Reader[E,T] with Writer[T] with Updater[E,T]

trait UserStore[U <: User] extends UserTypes[U] {
  
  def get(username: String): UserValidation
  
  def add(user: U): UserValidation
  
  def delete(username: String): UnitValidation
  
}

