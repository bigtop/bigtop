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


trait UserLogin[U <: User] extends UserCore[U] {

  def loginUser(username: String, password: String): UserValidation =
    for {
      user <- userStore.get(username)
      ans  <- if(user.isPasswordOk(password)) {
                user.success[Problem].fv
              } else {
                (Client.NoPassword : Problem).fail.fv
              }
    } yield ans

}
