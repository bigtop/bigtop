package bigtop.json

import bigtop.problem._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

case class JsonConfig(val data: JValue = JObject.empty) {
  def apply[T](path: JPath)(implicit reader: JsonReader[T]): T =
    get(path) match {
      case Success(value)  => value
      case Failure(errors) => throw JsonException(errors)
    }

  def get[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[T] =
    data.get(path) match {
      case JNothing => JsonErrors.Missing(path).fail[T]
      case json     => reader.read(json).fold(
                         succ = value => value.success[JsonErrors],
                         fail = errors => errors.prefix(path).fail[T]
                       )
    }

  def set[T](path: JPath, value: T)(implicit writer: JsonWriter[T]): JsonConfig =
    JsonConfig(data.set(path, writer.write(value)))

  def remove[T](path: JPath): JsonConfig =
    JsonConfig(data.set(path, JNothing).minimize.getOrElse(JObject.empty))

  def merge(that: JsonConfig): JsonConfig =
    JsonConfig {
      that.data.flattenWithPath.foldLeft(this.data) { (memo, kvp) =>
        memo.set(kvp._1, kvp._2)
      }
    }
}

object JsonConfig {
  val Empty = JsonConfig()

  implicit val monoid = new Monoid[JsonConfig] {
    def zero = JsonConfig.Empty
    def append(a: JsonConfig, b: => JsonConfig) = a merge b
  }
}