package bigtop
package user


import bigtop.concurrent._
import bigtop.concurrent.FutureImplicits._
import bigtop.json._
import bigtop.problem.Problem
import bigtop.problem.Problems._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz.Validation
import scalaz.syntax.validation._


trait UserActions[U <: User] extends UserTypes[U] {

  def updater:   JsonUpdater[Problem[String],U]
  /** Formatter for the *external* representation of the User. That is, the user as is displayed to the web browser. */
  def formatter: JsonFormat[Problem[String], U]

  def store: UserStore[U]

  def login(username: String, password: String): UserValidation =
    for {
      user <- store.get(username)
      ans  <- if(user.isPasswordOk(password)) {
                user.success[Problem[String]].fv
              } else {
                (Request.NoPassword : Problem[String]).fail.fv
              }
    } yield ans

  def create(data: JValue): UserValidation =
    for {
      unsavedUser <- formatter.read(data).fv
      savedUser   <- store.add(unsavedUser)
    } yield savedUser

  def read(username: String): UserValidation =
    for {
      user <- store.get(username)
    } yield user

  def update(username: String, data: JValue): UnitValidation =
    for {
      oldUser <- store.get(username)
      newUser <- updater.update(oldUser, data).fv
      savedUser <- store.add(newUser)
    } yield ()

  def delete(username: String): UnitValidation =
    store.delete(username)

  // def list(???): Seq[Unit]

}
