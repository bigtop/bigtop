package bigtop
package user

import bigtop.concurrent._
import bigtop.concurrent.FutureImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz.Validation
import scalaz.syntax.validation._


trait UserActions[U <: User] extends UserTypes[U] {

  /** The protocol writes the external representation of the user. It should omit fields such as the password that are not to be displayed publically */
  def protocol: Format[Error,U]
  def store: UserStore[U]

  def login(username: String, password: String): JsonValidation =
    for {
      user <- store.get(username)
      ans  <- if(user.isPasswordOk(password)) {
                protocol.write(user).success[Error].toValidationNel.fv
              } else {
                ("password" -> "Password is incorrect.").failNel.fv
              }
    } yield ans

  def create(data: JValue): JsonValidation =
    for {
      unsavedUser <- protocol.read(data).fv
      savedUser   <- store.add(unsavedUser)
    } yield protocol.write(savedUser)

  def read(username: String): JsonValidation =
    for {
      user <- store.get(username)
    } yield protocol.write(user)

  def update(username: String, data: JValue): UnitValidation =
    for {
      oldUser <- store.get(username)
      newUser <- protocol.update(oldUser, data).fv
      savedUser <- store.add(newUser)
    } yield ()

  def delete(username: String): UnitValidation =
    store.delete(username)

  // def list(???): Seq[Unit]

}
