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
      user <- readByUsername(username).mapFailure(exn => Problems.Authentication(username, cause = Some(exn)))
      ans  <- if(user.isPasswordOk(password)) {
                user.success[Problem].fv
              } else {
                Problems.Authentication(username).logMessage("Password incorrect.").fail.fv
              }
    } yield ans

  def create(user: U): UserValidation = {
    for {
      _     <- read(user.id).fold(
                 fail = { prob => ().success },
                 succ = { user => Problems.Exists("user").fail }
               ).fv
      _     <- readByUsername(user.username).fold(
                 fail = { prob => ().success },
                 succ = { user => Problems.Exists("user").fail }
               ).fv
      saved <- save(user)
    } yield saved
  }

  def update(user: U): UserValidation = {
    def exists(user: User) =
      (for {
        _ <- read(user.id)
      } yield ()).mapFailure(f => Problems.NotFound("user"))

    for {
      _     <- exists(user)
      saved <- save(user)
    } yield saved
  }
}
