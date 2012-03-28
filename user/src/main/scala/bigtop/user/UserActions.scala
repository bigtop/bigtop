package bigtop
package user

import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.json._
import bigtop.problem._
import bigtop.util.Uuid
import scalaz._
import scalaz.syntax.validation._

trait UserActions[U <: User] extends UserTypes[U] with FutureImplicits {
  implicit def internalFormat: JsonFormat[Problem,U]

  def read(id: Uuid): UserValidation

  def readByUsername(username: String): UserValidation

  def save(user: U): UserValidation

  def delete(id: Uuid): UnitValidation

  def login(username: String, password: String): UserValidation =
    for {
      user <- readByUsername(username).mapFailure(_ => Problems.Client.loginUsernameIncorrect)
      ans  <- if(user.isPasswordOk(password))
                user.success[Problem].fv
              else
                Problems.Client.loginPasswordIncorrect.fail.fv
    } yield ans

  def create(user: U): UserValidation = {
    def doesntExist(user: User) =
      (for {
        _ <- read(user.id).invert
        _ <- readByUsername(user.username).invert
      } yield ()).mapFailure(f => Problems.Client.exists("user"))

    for {
      _     <- doesntExist(user)
      saved <- save(user)
    } yield saved
  }

  def update(user: U): UserValidation = {
    def exists(user: User) =
      (for {
        _ <- read(user.id)
      } yield ()).mapFailure(f => Problems.Client.notFound("user"))

    for {
      _     <- exists(user)
      saved <- save(user)
    } yield saved
  }
}
