package bigtop.json
package format

import bigtop.concurrent._
import bigtop.problem._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

import JsonErrors.TypeError.{ apply => typeError }
import JsonErrors.Missing.{ apply => missing }

trait BaseFormats {
  implicit val uuidFormat: JsonFormat[Uuid] = new JsonFormat[Uuid] {
    def write(in: Uuid) = JString(in.toString)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Uuid.parse _).
        toSuccess(typeError("uuid", json))
  }

  implicit val emailFormat: JsonFormat[Email] = new JsonFormat[Email] {
    def write(in: Email) = JString(in.address)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Email.parse _).
        toSuccess(typeError("email", json))
  }

  implicit val passwordFormat: JsonFormat[Password] = new JsonFormat[Password] {
    def write(in: Password) = JString(in.hash)
    def read(json: JValue) = {
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Password.parseHash(_).toOption).
        toSuccess(typeError("password", json))
    }
  }

  implicit val stringFormat: JsonFormat[String] = new JsonFormat[String] {
    def write(in: String) = JString(in)
    def read(json: JValue) =
      (json -->? classOf[JString]).
      map(_.value).
      toSuccess(typeError("string", json))
  }

  implicit val booleanFormat: JsonFormat[Boolean] = new JsonFormat[Boolean] {
    def write(in: Boolean) = JBool(in)
    def read(json: JValue) =
      (json -->? classOf[JBool]).
      map(_.value).
      toSuccess(typeError("boolean", json))
  }

  implicit val dateTimeFormat: JsonFormat[DateTime] = new JsonFormat[DateTime] {
    def write(in: DateTime) = {
      // JInt(in.getMillis)
      JString(Iso8601Format.write(in))
    }

    def read(json: JValue) = {
      json match {
        // case JInt(bigInt) =>
        //   new DateTime(bigInt.toLong).success[JsonErrors]

        // case JDouble(bigDecimal) =>
        //   new DateTime(bigDecimal.toLong).success[JsonErrors]

        case JString(str) =>
          Iso8601Format.read(str) match {
            case Failure(str)  => typeError("time", json).fail[DateTime]
            case Success(date) => date.success[JsonErrors]
          }

        case _ =>
          typeError("time", json).fail[DateTime]
      }
    }
  }

  implicit val intFormat: JsonFormat[Int] = new JsonFormat[Int] {
    def isWholeNumber(num: Double) =
      math.abs(num - math.round(num)) < 0.01

    def write(in: Int) = JInt(in)

    def read(json: JValue) =
      (json -->? classOf[JInt] map (_.value.toInt)) orElse
      (json -->? classOf[JDouble] filter (json => isWholeNumber(json.value)) map (_.value.toInt)) toSuccess
      (typeError("int", json))
  }

  implicit val doubleFormat: JsonFormat[Double] = new JsonFormat[Double] {
    def write(in: Double) = JDouble(in)

    def read(json: JValue) =
      (json -->? classOf[JDouble]).
      map(_.value).
      toSuccess(typeError("double", json))
  }

  implicit val urlFormat: JsonWriter[URL] = new JsonWriter[URL] {
    def write(in: URL) = JString(in.toString)
  }

  implicit val identityFormat: JsonFormat[JValue] = new JsonFormat[JValue] {
    def write(in: JValue) = in
    def read(json: JValue) = json.success[JsonErrors]
  }
}
