package bigtop
package json

import bigtop.util._
import blueeyes.json.JsonAST._
import org.joda.time._

trait JsonFormatters {

  implicit val UuidJsonWriter: JsonWriter[Uuid] = new JsonWriter[Uuid] {
    def write(in: Uuid) = JString(in.toString)
  }

  implicit val StringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(in: String) = JString(in)
  }

  implicit val DateTimeJsonWriter: JsonWriter[DateTime] = new JsonWriter[DateTime] {
    def write(in: DateTime) = JString(Iso8601Format.write(in))
  }

  implicit val JValueJsonWriter: JsonWriter[JValue] = new JsonWriter[JValue] {
    def write(in: JValue) = in
  }

  case class JsonWritable[A](in: A) {
    def toJson(implicit w: JsonWriter[A]): JValue =
      w.write(in)
  }

  implicit def writableToJsonW[A](in: A): JsonWritable[A] =
    JsonWritable[A](in)
}

object JsonFormatters extends JsonFormatters
