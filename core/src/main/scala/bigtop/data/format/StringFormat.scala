package bigtop.data
package format

import bigtop.json._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import org.joda.time._
import scala.util.matching.{ Regex => ScalaRegex }
import scalaz._
import scalaz.Scalaz._

import bigtop.json.JsonErrors.Missing.{ apply => missing }
import bigtop.json.JsonErrors.TypeError.{ apply => typeError }

trait StringFormats {
  // Formats ------------------------------------

  implicit val uuidFormat: StringFormat[Uuid] =
    new StringFormat[Uuid] {
      def read(in: String) =
        Uuid.parse(in).toSuccess(typeError("uuid", in))

      def write(in: Uuid) =
        in.toString
    }

  implicit val uuidReader: StringReader[Uuid] = uuidFormat

  implicit val emailFormat: StringFormat[Email] =
    new StringFormat[Email] {
      def read(in: String) =
        Email.parse(in).toSuccess(typeError("email", in))

      def write(in: Email) =
        in.toString
    }

  implicit val emailReader: StringReader[Email] = emailFormat

  implicit val booleanFormat: StringFormat[Boolean] =
    new StringFormat[Boolean] {
      def read(in: String) =
        in.toLowerCase match {
          case "true"  => true.success[JsonErrors]
          case "false" => false.success[JsonErrors]
          case _       => typeError("boolean", in).fail[Boolean]
        }

      def write(in: Boolean) =
        in.toString
    }

  implicit val booleanReader: StringReader[Boolean] = booleanFormat

  implicit val intFormat: StringFormat[Int] =
    new StringFormat[Int] {
      def read(in: String) =
        try {
          in.toInt.success[JsonErrors]
        } catch {
          case exn: NumberFormatException =>
            typeError("integer", in).fail[Int]
        }

      def write(in: Int) =
        in.toString
    }

  implicit val intReader: StringReader[Int] = intFormat

  implicit val doubleFormat: StringFormat[Double] =
    new StringFormat[Double] {
      def read(in: String) =
        try {
          in.toDouble.success[JsonErrors]
        } catch {
          case exn: NumberFormatException =>
            typeError("double", in).fail[Double]
        }

      def write(in: Double) =
        in.toString
    }

  implicit val doubleReader: StringReader[Double] = doubleFormat

  implicit val dateTimeFormat: StringFormat[DateTime] =
    new StringFormat[DateTime] {
      def read(in: String) =
        Iso8601Format.read(in).bimap(
          g = dt => dt,
          f = in => typeError("ISO8601 date/time (%s)".format(Iso8601Format.millisFormatString), in)
        )

      def write(in: DateTime) =
        Iso8601Format.write(in)
    }

  implicit val dateTimeReader: StringReader[DateTime] = dateTimeFormat

  // Validators ---------------------------------

  val trim = new Validator[String] {
    def read(in: String) =
      in.trim.success[JsonErrors]
    def write(in: String) = in
  }

  val nonBlank = new Validator[String] {
    def read(in: String) =
      if(in.length > 0) {
        in.success[JsonErrors]
      } else {
        missing().fail[String]
      }
    def write(in: String) = in
  }

  val lowerCase = new Validator[String] {
    def read(in: String) =
      in.toLowerCase.success[JsonErrors]
    def write(in: String) = in
  }

  val upperCase = new Validator[String] {
    def read(in: String) =
      in.toUpperCase.success[JsonErrors]
    def write(in: String) = in
  }

  def maxLength(length: Int) = new Validator[String] {
    def read(in: String) =
      if(in.length <= length) {
        in.success[JsonErrors]
      } else {
        typeError("maximum length " + length, in).fail
      }
    def write(in: String) = in
  }

  def minLength(length: Int) = new Validator[String] {
    def read(in: String) =
      if(in.length >= length) {
        in.success[JsonErrors]
      } else {
        typeError("minimum length " + length, in).fail
      }
    def write(in: String) = in
  }

  def regex(rx: ScalaRegex, expected: String) = new Validator[String] {
    def read(in: String) =
      if(rx.findFirstIn(in).isDefined) {
        in.success[JsonErrors]
      } else {
        typeError(expected, in).fail
      }
    def write(in: String) = in
  }

  val email = regex("^[^@]+@[^@]+$".r, "email")
}
