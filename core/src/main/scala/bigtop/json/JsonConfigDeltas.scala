package bigtop.json

import bigtop.json.JsonFormatters._
import bigtop.problem._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

case class JsonConfigDeltas(val deltas: List[(JPath, JValue)]) {
  def +: (delta: (JPath, JValue)) =
    JsonConfigDeltas(delta +: deltas)

  def :+ (delta: (JPath, JValue)) =
    JsonConfigDeltas(deltas :+ delta)

  def ++ (that: JsonConfigDeltas) =
    JsonConfigDeltas(this.deltas ++ that.deltas)
}

object JsonConfigDeltas {
  val Empty = JsonConfigDeltas(Nil)

  def apply(deltas: (JPath, JValue) *): JsonConfigDeltas =
    JsonConfigDeltas(deltas.toList)

  def apply(deltas: JObject): JsonConfigDeltas =
    JsonConfigDeltas(deltas.fields.map {
      case JField(name, value) =>
        JPath(name) -> value
    })

  implicit object format extends JsonFormat[JsonConfigDeltas] {
    def write(in: JsonConfigDeltas) =
      JObject(in.deltas.map { delta =>
        JField(delta._1.toString, delta._2)
      })

    def read(in: JValue) =
      in match {
        case obj: JObject =>
          apply(obj).success[JsonErrors]

        case other =>
          JsonErrors.Malformed(
            "",
            "Expected map of paths to new values, received " + other
          ).fail[JsonConfigDeltas]
      }
  }

  implicit object monoid extends Monoid[JsonConfigDeltas] {
    def zero = JsonConfigDeltas.Empty
    def append(a: JsonConfigDeltas, b: => JsonConfigDeltas) = a ++ b
  }
}