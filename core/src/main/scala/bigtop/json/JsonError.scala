package bigtop.json

import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.JsonFormats._
import bigtop.problem._
import scalaz._
import scalaz.Scalaz._

case class JsonError(
  val errorType: String, // machine-readable error message
  val path: JPath,       // path (as accepted by JValue.{get,set})
  val message: String,   // human-readable error message
  val data: JValue       // extra data
) {
  def prefix(prefix: JPath) =
    JsonError( errorType, prefix \ path, message, data)
}

object JsonError {
  object Missing {
    def apply(path: JPath, message: String = "This value is required.", data: JValue = JNothing) =
      JsonError("missing", path, message, data)

    def unapply(in: JsonError) =
      if(in.errorType == "missing") Some((in.path, in.message, in.data)) else None
  }

  object Malformed {
    def apply(path: JPath, message: String, data: JValue = JNothing) =
      JsonError("malformed", path, message, data)

    def unapply(in: JsonError) =
      if(in.errorType == "malformed") Some((in.path, in.message, in.data)) else None
  }

  object TypeError {
    def apply(path: JPath, expected: String, actual: JValue) =
      JsonError("malformed", path, "Expected " + expected + ", received " + actual, JNothing)
  }

  // implicit def jsonErrorToJsonErrors(in: JsonError) =
  //   JsonErrors(List(in))

  implicit val format: JsonFormat[JsonError] = tupleFormat(
    (__ \ "typename").required[String],
    (__ \ "path"    ).nullable[JPath](JPath.Identity),
    (__ \ "message" ).required[String],
    (__ \ "data"    ).nullable[JValue](JNothing)
  )(Function.tupled(JsonError.apply), Function.unlift(JsonError.unapply))

  // implicit object format extends JsonFormat[JsonError] {
  //   def read(in: JValue) = {
  //     for {
  //       errorType <- in.mandatory[String]("typename")
  //       path      <- in.optional[String]("path", "")
  //       message   <- in.mandatory[String]("message")
  //       data      <- in.optional[JValue]("data", JNothing)
  //     } yield JsonError(errorType, path, message, data)
  //   }

  //   def write(in: JsonError) = {
  //     ("typename" -> in.errorType) ~
  //     ("path"     -> in.path) ~
  //     ("message"  -> in.message) ~
  //     ("data"     -> in.data)
  //   }
  // }
}
