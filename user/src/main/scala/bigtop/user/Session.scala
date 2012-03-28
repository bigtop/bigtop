package bigtop
package user

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.JsonWriter
import bigtop.json.JsonFormatters
import bigtop.util.Uuid
import scala.collection.mutable.Map

case class Session[U](
  val id: Uuid,
  val realUser: U,
  val effectiveUser: U,
  val session: Map[String, JValue]
)

object Session {
  trait SessionWriter[U] extends JsonWriter[Session[U]] with JsonFormatters {
    def userWriter: JsonWriter[U]

    implicit def write(session: Session[U]) = {
      ("typename" -> "session") ~
      ("id"       -> session.id.toJson) ~
      ("session"  -> session.session.toList.map(pair => JField(pair._1, pair._2))) ~
      ("user"     -> userWriter.write(session.effectiveUser))
    }
  }

  def externalWriter[U](uWriter: JsonWriter[U]) = {
    new SessionWriter[U] {
      val userWriter = uWriter
    }
  }
}
