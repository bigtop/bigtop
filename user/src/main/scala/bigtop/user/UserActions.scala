package bigtop
package user

import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.json._
import bigtop.problem._
import bigtop.util.Uuid
import scalaz._
import scalaz.syntax.validation._

trait UserActions[U <: User] extends UserLogin[U]
    with UserCreate[U]
    with UserRead[U]
    with UserUpdate[U]
    with UserDelete[U]

trait UserAction[U <: User] extends UserTypes[U] with FutureImplicits {
  def externalFormat: JsonUpdater[Problem, U]
  def store: UserStore[U]
}

trait UserLogin[U <: User] extends UserAction[U] {
  def login(username: String, password: String): UserValidation =
    for {
      user <- store.searchByUsername(username).mapFailure(_ => Problems.Client.loginUsernameIncorrect)
      ans  <- if(user.isPasswordOk(password)) {
                user.success[Problem].fv
              } else Problems.Client.loginPasswordIncorrect.fail.fv
    } yield ans
}

trait UserCreate[U <: User] extends UserAction[U] {
  def create(data: JValue): UserValidation = {
    def doesntExist(user: User) =
      (for {
        _ <- store.read(user.id).invert
        _ <- store.searchByUsername(user.username).invert
      } yield ()).mapFailure(f => Problems.Client.exists("user"))

    for {
      unsavedUser <- externalFormat.read(data).fv
      _           <- doesntExist(unsavedUser)
      savedUser   <- store.create(unsavedUser)
    } yield savedUser
  }
}

trait UserRead[U <: User] extends UserAction[U] {
  def read(id: Uuid): UserValidation =
    store.read(id)
}

trait UserUpdate[U <: User] extends UserAction[U] {
  def update(id: Uuid, data: JValue): UnitValidation =
    for {
      oldUser   <- store.read(id)
      newUser   <- externalFormat.update(oldUser, data).fv
      savedUser <- store.update(newUser)
    } yield ()
}

trait UserDelete[U <: User] extends UserAction[U] {
  def delete(id: Uuid): UnitValidation =
    store.delete(id)
}
