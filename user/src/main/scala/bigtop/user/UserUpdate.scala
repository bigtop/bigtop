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

trait UserUpdate[U <: User] extends UserTypes[U] {

  def core: UserCore[U]

  def update(username: String, data: JValue): UnitValidation =
    for {
      oldUser <- core.store.read(username)
      newUser <- core.serializer.update(oldUser, data).fv
      savedUser <- core.store.update(newUser)
    } yield ()

}
