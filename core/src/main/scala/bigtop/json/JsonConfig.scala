package bigtop.json

import bigtop.problem._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

case class JsonConfig(val data: JValue = JObject.empty) {
  def apply[T](key: String)(implicit reader: JsonReader[Problem, T]): T =
    get(key) match {
      case Success(value)   => value
      case Failure(problem) => throw problem
    }

  def get[T](key: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem, T] =
    data.get(key) match {
      case JNothing => Problems.Missing(key).fail[T]
      case json     => reader.read(json).fold(
                         succ = { value =>
                           value.success[Problem]
                         },
                         fail = { problem =>
                           Problems.Malformed(
                             field       = key,
                             description = "expected " + reader.valueTypeName + ", found " + json,
                             cause       = Some(problem)
                           ).fail[T]
                         }
                       )
    }

  def set[T](key: String, value: T)(implicit writer: JsonWriter[T]): JsonConfig =
    JsonConfig(data.set(key, writer.write(value)))

  def remove[T](key: String): JsonConfig =
    JsonConfig(data.set(key, JNothing).minimize.getOrElse(JObject.empty))
}
