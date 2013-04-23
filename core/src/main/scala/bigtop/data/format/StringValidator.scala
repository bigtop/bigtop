package bigtop.data
package format

import bigtop.json._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scala.util.matching.{ Regex => ScalaRegex }
import scalaz._
import scalaz.Scalaz._

import bigtop.json.JsonErrors.Missing.{ apply => missing }
import bigtop.json.JsonErrors.TypeError.{ apply => typeError }

trait StringValidators {
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
