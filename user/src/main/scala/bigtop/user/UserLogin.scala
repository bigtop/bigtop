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

trait UserLogin[U <: User] extends UserTypes[U] {

  def core: UserCore[U]

  def login(username: String, password: String): UserValidation =
    for {
      user <- core.store.read(username).mapFailure(_ => Client.loginIncorrect)
      ans  <- if(user.isPasswordOk(password)) {
                user.success[Problem].fv
              } else Client.loginIncorrect.fail.fv
    } yield ans

}
