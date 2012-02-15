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


trait UserUpdate[U <: User] extends UserCore[U] {

  def updateUser(username: String, data: JValue): UnitValidation =
    for {
      oldUser <- userStore.get(username)
      newUser <- userUpdater.update(oldUser, data).fv
      savedUser <- userStore.add(newUser)
    } yield ()

}
