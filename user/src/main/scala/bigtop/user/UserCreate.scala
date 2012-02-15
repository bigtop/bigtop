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

trait UserCreate[U <: User] extends UserCore[U] {

  def createUser(data: JValue): UserValidation =
    for {
      unsavedUser <- userFormatter.read(data).fv
      savedUser   <- userStore.get(unsavedUser.username).invert.mapFailure(_ => Client.exists("user")).flatMap(_ => userStore.add(unsavedUser))
    } yield savedUser

}
