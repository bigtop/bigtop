package bigtop
package util

import scala.util.matching.Regex
import scalaz.Validation
import scalaz.syntax.validation._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

case class Uuid(val name: String) {
  override def toString: String = name
}

object Uuid {
  private val UuidRegex = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$".r

  def create() =
    Uuid(java.util.UUID.randomUUID.toString)

  def parse(name: String): Option[Uuid] =
    name match {
      case UuidRegex() => Some(Uuid(name))
      case _           => None
    }

  object Parse {
    def unapply(name: String): Option[Uuid] = {
      parse(name)
    }
  }
}
