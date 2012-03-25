package bigtop
package user

import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.json._
import bigtop.problem._
import bigtop.util.Uuid
import scalaz._
import scalaz.syntax.validation._

// Interface

trait UserActions[U <: User] extends UserTypes[U] {

  def store: UserStore[U]

  def externalFormat: JsonUpdater[Problem,U]

  def login(username: String, password: String): UserValidation

  def create(data: JValue): UserValidation

  def read(id: Uuid): UserValidation

  def update(id: Uuid, data: JValue): UnitValidation

  def delete(id: Uuid): UnitValidation

}


// Default implementations

case class UserActionsBuilder[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U]) extends UserActions[U] {

  val userLogin  = UserLogin[U](store,  externalFormat)
  val userCreate = UserCreate[U](store, externalFormat)
  val userRead   = UserRead[U](store,   externalFormat)
  val userUpdate = UserUpdate[U](store, externalFormat)
  val userDelete = UserDelete[U](store, externalFormat)

  def login(username: String, password: String): UserValidation =
    userLogin.login(username, password)

  def create(data: JValue): UserValidation =
    userCreate.create(data)

  def read(id: Uuid): UserValidation =
    userRead.read(id)

  def update(id: Uuid, data: JValue): UnitValidation =
    userUpdate.update(id, data)

  def delete(id: Uuid): UnitValidation =
    userDelete.delete(id)

}

trait UserAction[U <: User] extends UserTypes[U] with FutureImplicits {

  def externalFormat: JsonUpdater[Problem,U]

}

case class UserLogin[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U]) extends UserAction[U] {
  def login(username: String, password: String): UserValidation =
    for {
      user <- store.searchByUsername(username).mapFailure(_ => Problems.Client.loginUsernameIncorrect)
      ans  <- if(user.isPasswordOk(password))
                user.success[Problem].fv
              else
                Problems.Client.loginPasswordIncorrect.fail.fv
    } yield ans
}

case class UserCreate[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U]) extends UserAction[U] {
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

case class UserRead[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U]) extends UserAction[U] {
  def read(id: Uuid): UserValidation =
    store.read(id)
}

case class UserUpdate[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U])  extends UserAction[U] {
  def update(id: Uuid, data: JValue): UnitValidation =
    for {
      oldUser   <- store.read(id)
      newUser   <- externalFormat.update(oldUser, data).fv
      savedUser <- store.update(newUser)
    } yield ()
}

case class UserDelete[U <: User](val store: UserStore[U], val externalFormat: JsonUpdater[Problem,U]) extends UserAction[U] {
  def delete(id: Uuid): UnitValidation =
    store.delete(id)
}
