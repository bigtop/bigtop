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


trait UserDelete[U <: User] extends UserCore[U] {

  def deleteUser(username: String): UnitValidation =
    userStore.delete(username)

}
