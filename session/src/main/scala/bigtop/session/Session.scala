package bigtop
package session


import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.JsonWriter
import bigtop.json.JsonFormatters
import bigtop.util.Uuid
import scala.collection.mutable.Map


case class Session[U](val id: Uuid,
                      val user: U,
                      val session: Map[String, JValue])
                     (implicit val userWriter: JsonWriter[U])

trait SessionWriter[U] extends JsonWriter[Session[U]] with JsonFormatters {

  implicit def write(session: Session[U]) = {
    ("typename" -> "session") ~
    ("id" -> session.id.toJson) ~
    ("session" -> session.session.toList.map(pair => JField(pair._1, pair._2))) ~
    ("user" -> session.userWriter.write(session.user))
  }

}
