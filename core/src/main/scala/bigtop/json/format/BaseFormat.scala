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
  implicit object uuidFormat extends UuidFormat
  implicit object jPathFormat extends JPathFormat
  implicit object emailFormat extends EmailFormat
  implicit object passwordFormat extends PasswordFormat
  implicit object stringFormat extends StringFormat
  implicit object booleanFormat extends BooleanFormat
  implicit object dateTimeFormat extends DateTimeFormat
  implicit object intFormat extends IntFormat
  implicit object doubleFormat extends DoubleFormat
  implicit object urlFormat extends UrlFormat
  implicit object jValueFormat extends JValueFormat
}

trait BaseReaders {
  implicit object uuidReader extends UuidReader
  implicit object jPathReader extends JPathReader
  implicit object emailReader extends EmailReader
  implicit object passwordReader extends PasswordReader
  implicit object stringReader extends StringReader
  implicit object booleanReader extends BooleanReader
  implicit object dateTimeReader extends DateTimeReader
  implicit object intReader extends IntReader
  implicit object doubleReader extends DoubleReader
  implicit object urlReader extends UrlReader
  implicit object jValueReader extends JValueReader
}

trait BaseWriters {
  implicit object uuidWriter extends UuidWriter
  implicit object jPathWriter extends JPathWriter
  implicit object emailWriter extends EmailWriter
  implicit object passwordWriter extends PasswordWriter
  implicit object stringWriter extends StringWriter
  implicit object booleanWriter extends BooleanWriter
  implicit object dateTimeWriter extends DateTimeWriter
  implicit object intWriter extends IntWriter
  implicit object doubleWriter extends DoubleWriter
  implicit object urlWriter extends UrlWriter
  implicit object jValueWriter extends JValueWriter
}

trait UuidFormat extends UuidReader with UuidWriter with JsonFormat[Uuid]

trait UuidReader extends JsonReader[Uuid] {
  def read(in: JValue) =
    in match {
      case JString(Uuid.Parse(in)) => in.success[JsonErrors]
      case _ => typeError("uuid", in).fail[Uuid]
    }
}

trait UuidWriter extends JsonWriter[Uuid] {
  def write(in: Uuid) =
    JString(in.toString)
}

trait JPathFormat extends JPathReader with JPathWriter with JsonFormat[JPath]

trait JPathReader extends JsonReader[JPath] {
  def read(in: JValue) =
    in match {
      case JString(in) => JPath(in).success[JsonErrors]
      case _ => typeError("json path", in).fail[JPath]
    }
}

trait JPathWriter extends JsonWriter[JPath] {
  def write(in: JPath) =
    JString(in.toString)
}

trait EmailFormat extends EmailReader with EmailWriter with JsonFormat[Email]

trait EmailReader extends JsonReader[Email] {
  def read(in: JValue) =
    in match {
      case JString(Email.Parse(in)) => in.success[JsonErrors]
      case _ => typeError("email", in).fail[Email]
    }
}

trait EmailWriter extends JsonWriter[Email] {
  def write(in: Email) =
    JString(in.address)
}

trait PasswordFormat extends PasswordReader with PasswordWriter with JsonFormat[Password]

trait PasswordReader extends JsonReader[Password] {
  def read(json: JValue) =
    (json -->? classOf[JString]).
      map(_.value).
      flatMap(Password.parseHash(_).toOption).
      toSuccess(typeError("password", json))
}

trait PasswordWriter extends JsonWriter[Password] {
  def write(in: Password) = JString(in.hash)
}

trait StringFormat extends StringReader with StringWriter with JsonFormat[String]

trait StringReader extends JsonReader[String] {
  def read(json: JValue) =
    (json -->? classOf[JString]).
    map(_.value).
    toSuccess(typeError("string", json))
}

trait StringWriter extends JsonWriter[String] {
  def write(in: String) = JString(in)
}

trait BooleanFormat extends BooleanReader with BooleanWriter with JsonFormat[Boolean]

trait BooleanReader extends JsonReader[Boolean] {
  def read(json: JValue) =
    (json -->? classOf[JBool]).
    map(_.value).
    toSuccess(typeError("boolean", json))
}

trait BooleanWriter extends JsonWriter[Boolean] {
  def write(in: Boolean) = JBool(in)
}

trait DateTimeFormat extends DateTimeReader with DateTimeWriter with JsonFormat[DateTime]

trait DateTimeReader extends JsonReader[DateTime] {
  def read(json: JValue) =
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

trait DateTimeWriter extends JsonWriter[DateTime] {
  def write(in: DateTime) =
    // JInt(in.getMillis)
    JString(Iso8601Format.write(in))
}

trait IntFormat extends IntReader with IntWriter with JsonFormat[Int]

trait IntReader extends JsonReader[Int] {
  def isWholeNumber(num: Double) =
    math.abs(num - math.round(num)) < 0.01

  def read(json: JValue) =
    (json -->? classOf[JInt] map (_.value.toInt)) orElse
    (json -->? classOf[JDouble] filter (json => isWholeNumber(json.value)) map (_.value.toInt)) toSuccess
    (typeError("int", json))
}

trait IntWriter extends JsonWriter[Int] {
  def write(in: Int) = JInt(in)
}

trait DoubleFormat extends DoubleReader with DoubleWriter with JsonFormat[Double]

trait DoubleReader extends JsonReader[Double] {
  def read(json: JValue) =
    (json -->? classOf[JDouble]).
    map(_.value).
    toSuccess(typeError("double", json))
}

trait DoubleWriter extends JsonWriter[Double] {
  def write(in: Double) = JDouble(in)
}

trait UrlFormat extends UrlReader with UrlWriter with JsonFormat[URL]

trait UrlReader extends JsonReader[URL] {
  def read(in: URL) =
    try {
      URL(in).success[JsonErrors]
    } catch {
      case exn: MalformedURLException =>
        typeError("url", in).fail[URL]
    }
}

trait UrlWriter extends JsonWriter[URL] {
  def write(in: URL) = JString(in.toString)
}

trait JValueFormat extends JValueReader with JValueWriter with JsonFormat[JValue]

trait JValueReader extends JsonReader[JValue] {
  def read(in: JValue) = in.success[JsonErrors]
}

trait JValueWriter extends JsonWriter[JValue] {
  def write(in: JValue) = in
}
