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

  val Empty = JsonErrors(Nil)

  object Missing {
    def apply(): JsonErrors =
      apply(JPath.Identity, "This value is required.", JNothing)

    def apply(path: JPath, message: String = "This value is required.", data: JValue = JNothing): JsonErrors =
      JsonErrors(JsonError("missing", path, message, data))
  }

  object Malformed {
    def apply(path: JPath, message: String = "This value is in the wrong format.", data: JValue = JNothing): JsonErrors =
      JsonErrors(JsonError("malformed", path, message, data))
  }

  object TypeError {
    def apply(expected: String, actual: JValue): JsonErrors =
      apply(JPath.Identity, expected, actual)

    def apply(path: JPath, expected: String, actual: JValue): JsonErrors =
      JsonErrors(JsonError(
        "malformed",
        path,
        "expected " + expected + ", found " + actual,
        ("expected" -> expected) ~ ("actual" -> actual)
      ))
  }

  implicit object format extends JsonFormat[JsonErrors] {
    implicit val errorsFormat = seqFormat[JsonError]

    def read(in: JValue) =
      in.asSeq[JsonError].map(errors => JsonErrors(errors.toList))

    def write(in: JsonErrors) =
      JArray(in.errors.map(_.toJson))
  }

  implicit object monoid extends Monoid[JsonErrors] {
    val zero = JsonErrors.Empty
    def append(a: JsonErrors, b: => JsonErrors): JsonErrors = a ++ b
  }
}
