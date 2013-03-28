package bigtop.json

import bigtop.problem._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

case class JsonConfig(val data: JValue = JObject.empty) {
  def apply[T](key: String)(implicit reader: JsonReader[T]): T =
    get(key) match {
      case Success(value)  => value
      case Failure(errors) => throw JsonException(errors)
    }

  def get[T](key: String)(implicit reader: JsonReader[T]): JsonValidation[T] =
    data.get(key) match {
      case JNothing => JsonErrors.Missing(key).fail[T]
      case json     => reader.read(json).fold(
                         succ = value => value.success[JsonErrors],
                         fail = errors => errors.prefix(key).fail[T]
                       )
    }

  def set[T](key: String, value: T)(implicit writer: JsonWriter[T]): JsonConfig =
    JsonConfig(data.set(key, writer.write(value)))

  def remove[T](key: String): JsonConfig =
    JsonConfig(data.set(key, JNothing).minimize.getOrElse(JObject.empty))
}
