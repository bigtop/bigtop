package bigtop
package util

import scala.util.matching.Regex
import scalaz.Validation
import scalaz.syntax.validation._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

case class Email(val address: String) {
  override def toString = address
}

object Email {
  private val EmailRegex = "^[^@]+@[^@]+$".r

  def parse(address: String): Option[Email] =
    address match {
      case EmailRegex() => Some(Email(address))
      case _            => None
    }

  object Parse {
    def unapply(address: String): Option[Email] = {
      parse(address)
    }
  }
}
