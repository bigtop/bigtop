package bigtop
package problem

import akka.dispatch.{ExecutionContext, Promise}
import blueeyes.bkka.AkkaDefaults
import bigtop.concurrent._
import bigtop.problem._
import scala.util.matching.Regex
import scalaz.Validation
import scalaz.syntax.validation._

trait StringImplicits {
  implicit def stringValidationToStringValidation(sv: Validation[Problem,String]) =
    StringValidation(sv)
}

case class StringValidation(val inner: Validation[Problem,String]) extends AkkaDefaults {
  def trim =
    StringValidation(inner.map(_.trim))

  def nonBlank(field: String) =
    inner.flatMap { str =>
      if(str.length > 0) {
        str.success[Problem]
      } else {
        Problems.Client.missing(field).fail
      }
    }

  def lowercase =
    inner.map(_.toLowerCase)

  def uppercase =
    inner.map(_.toLowerCase)

  def shorterThan(field: String, length: Int) =
    inner.flatMap(
      str =>
        if(str.length < length)
          str.success[Problem]
        else
          Problems.Client.malformed(field, "tooLong").fail
    )

  def longerThan(field: String, length: Int) =
    inner.flatMap(
      str =>
        if(str.length > length)
          str.success[Problem]
        else
          Problems.Client.malformed(field, "tooShort").fail
    )

  def regex(rx: Regex, field: String, description: String) =
    inner.flatMap { str =>
      if(rx.findFirstIn(str).isDefined) {
        str.success[Problem]
      } else {
        Problems.Client.malformed(field, description).fail
      }
    }

  def sv: StringValidation =
    this

  def fv: FutureValidation[Problem,String] =
    FutureValidation(Promise.successful(inner))
}
