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

trait UserCore[U <: User] extends UserTypes[U] {

  /** Formatter and updater for the *external* representation of the User. That is, the user as is displayed to the web browser. */
  def serializer: JsonFormat[Problem, U] with JsonUpdater[Problem, U]

  def store: UserStore[U]

}
