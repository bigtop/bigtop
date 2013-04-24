package bigtop.json

import bigtop.json.JsonFormatters._
import bigtop.problem._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

case class JsonConfigDeltas(val deltas: List[(JPath, JValue)]) {
  def isEmpty = deltas.isEmpty

  def +: (delta: (JPath, JValue)) =
    JsonConfigDeltas(delta +: deltas)

  def :+ (delta: (JPath, JValue)) =
    JsonConfigDeltas(deltas :+ delta)

  def ++ (that: JsonConfigDeltas) =
    JsonConfigDeltas(this.deltas ++ that.deltas)

  def apply(config: JsonConfig) =
    JsonConfig(deltas.foldLeft(config.data) { (data, delta) =>
      delta._2 match {
        case JNull    => data.set(delta._1, JNothing)
        case JNothing => data.set(delta._1, JNothing)
        case _        => data.set(delta._1, delta._2)
      }
    })
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
      if(in.deltas.isEmpty) {
        JNothing
      } else {
        JObject(in.deltas.map { delta =>
          JField(delta._1.toString, delta._2)
        })
      }

    def read(in: JValue) =
      in match {
        case obj: JObject =>
          apply(obj).success[JsonErrors]

        case other =>
          JsonErrors.TypeError("object", other).fail[JsonConfigDeltas]
      }
  }

  implicit object monoid extends Monoid[JsonConfigDeltas] {
    def zero = JsonConfigDeltas.Empty
    def append(a: JsonConfigDeltas, b: => JsonConfigDeltas) = a ++ b
  }
}