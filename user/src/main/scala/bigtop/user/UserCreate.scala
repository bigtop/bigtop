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

trait UserCreate[U <: User] extends UserTypes[U] {

  def core: UserCore[U]

  def create(data: JValue): UserValidation =
    for {
      unsavedUser <- core.serializer.read(data).fv
      savedUser   <- core.store.read(unsavedUser.username).invert.mapFailure(_ => Client.exists("user")).flatMap(_ => core.store.create(unsavedUser))
    } yield savedUser

}
