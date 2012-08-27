package bigtop
package util

import java.util.UUID
import scala.util.matching.Regex
import scalaz.Validation
import scalaz.syntax.validation._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

// case class Uuid(val name: String) {
//   override def toString: String = name
// }

object Uuid {
  private val UuidRegex = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$".r

  def create = UUID.randomUUID

  def apply(name: String) = UUID.fromString(name)

  def unapply(uuid: Uuid): Option[String] =
    Some(uuid.toString)

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
