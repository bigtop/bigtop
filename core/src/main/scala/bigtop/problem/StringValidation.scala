package bigtop
package problem

import akka.dispatch.{ExecutionContext, Promise}
import blueeyes.bkka.AkkaDefaults
import bigtop.concurrent._
import bigtop.json._
import bigtop.problem._
import scala.util.matching.Regex
import scalaz._
import scalaz.Scalaz._

trait StringImplicits {
  implicit def stringToStringValidation(str: String) =
    StringValidation(str.success[JsonErrors])

  implicit def stringValidationToStringValidation(sv: JsonValidation[String]) =
    StringValidation(sv)
}

case class StringValidation(val inner: JsonValidation[String]) extends AkkaDefaults {
  def trim =
    StringValidation(inner.map(_.trim))

  def nonBlank(field: String) =
    inner.flatMap { str =>
      if(str.length > 0) {
        str.success[JsonErrors]
      } else {
        JsonErrors.Missing(field).fail
      }
    }

  def lowercase =
    inner.map(_.toLowerCase)

  def uppercase =
    inner.map(_.toLowerCase)

  def shorterThan(field: String, length: Int) =
    inner.flatMap(
      str =>
        if(str.length < length) {
          str.success[JsonErrors]
        } else {
          JsonErrors.Malformed(field, "tooLong").fail
        }
    )

  def longerThan(field: String, length: Int) =
    inner.flatMap(
      str =>
        if(str.length > length) {
          str.success[JsonErrors]
        } else {
          JsonErrors.Malformed(field, "tooShort").fail
        }
    )

  def regex(rx: Regex, field: String, description: String) =
    inner.flatMap { str =>
      if(rx.findFirstIn(str).isDefined) {
        str.success[JsonErrors]
      } else {
        JsonErrors.Malformed(field, description).fail
      }
    }

  def email(field: String) =
    regex("^[^@]+@[^@]+$".r, field, "notEmail")

  def sv: StringValidation =
    this
}
