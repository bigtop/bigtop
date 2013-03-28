package bigtop.json

import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.JsonFormatters._
import bigtop.problem._
import scalaz._
import scalaz.Scalaz._

case class JsonErrors(val errors: List[JsonError]) {
  def ++(that: JsonErrors) =
    JsonErrors(this.errors ++ that.errors)

  def :+(error: JsonError) =
    JsonErrors(this.errors :+ error)

  def +:(error: JsonError) =
    JsonErrors(error +: this.errors)

  def isEmpty =
    errors.isEmpty

  def prefix(prefix: JPath): JsonErrors =
    JsonErrors(errors map (_ prefix prefix))

  def get(path: JPath): Option[JsonError] =
    errors filter (_.path == path) headOption

  def all(path: JPath): List[JsonError] =
    errors filter (_.path == path)
}

object JsonErrors {
  def apply(errors: JsonError *): JsonErrors =
    JsonErrors(errors.toList)

  object Missing {
    def apply(path: JPath, message: String = "This value is required.", data: JValue = JNothing): JsonErrors =
      JsonErrors(JsonError("missing", path, message, data))
  }

  object Malformed {
    def apply(path: JPath, message: String = "This value is in the wrong format.", data: JValue = JNothing): JsonErrors =
      JsonErrors(JsonError("malformed", path, message, data))
  }

  implicit object format extends  JsonFormat[JsonErrors] {
    implicit val errorsFormat = buildSeqFormat[JsonError]

    def read(in: JValue) =
      for {
        errors <- in.as[Seq[JsonError]]
      } yield JsonErrors(errors.toList)

    def write(in: JsonErrors) =
      JArray(in.errors.map(_.toJson))
  }

  implicit val semigroup =
     new Semigroup[JsonErrors] {
       def append(a: JsonErrors, b: => JsonErrors): JsonErrors = a ++ b
     }
}
