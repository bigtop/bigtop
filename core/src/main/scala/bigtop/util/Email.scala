package bigtop
package util

import scala.util.matching.Regex
import scalaz.Validation
import scalaz.syntax.validation._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

case class Email(val email: String)

object Email {
  private val EmailRegex = "^[^@]+@[^@]+$".r

  def parse(email: String): Option[Email] =
    email match {
      case EmailRegex() => Some(Email(email))
      case _            => None
    }
}
