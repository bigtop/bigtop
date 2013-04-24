package bigtop
package problem

// import akka.dispatch.{ExecutionContext, Promise}
// import blueeyes.bkka.AkkaDefaults
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.concurrent._
import bigtop.json._
import bigtop.problem._
import scala.util.matching.Regex
import scalaz._
import scalaz.Scalaz._

object StringImplicits extends StringImplicits

trait StringImplicits {
  implicit def stringToStringValidation(str: String) =
    StringValidation(str.success[JsonErrors])

  implicit def stringValidationToStringValidation(sv: JsonValidation[String]) =
    StringValidation(sv)
}

case class StringValidation(val inner: JsonValidation[String]) {
  def trim =
    StringValidation(inner.map(_.trim))

  def nonBlank(path: JPath) =
    inner.flatMap { str =>
      if(str.length > 0) {
        str.success[JsonErrors]
      } else {
        JsonErrors.Missing(path).fail
      }
    }

  def lowercase =
    inner.map(_.toLowerCase)

  def uppercase =
    inner.map(_.toLowerCase)

  def shorterThan(path: JPath, length: Int) =
    inner.flatMap { str =>
      if(str.length < length) {
        str.success[JsonErrors]
      } else {
        JsonErrors.TypeError(path, "max length " + (length - 1), str).fail
      }
    }

  def longerThan(path: JPath, length: Int) =
    inner.flatMap { str =>
      if(str.length > length) {
        str.success[JsonErrors]
      } else {
        JsonErrors.TypeError(path, "min length " + (length + 1), str).fail
      }
    }

  def regex(rx: Regex, path: JPath, expected: String) =
    inner.flatMap { str =>
      if(rx.findFirstIn(str).isDefined) {
        str.success[JsonErrors]
      } else {
        JsonErrors.TypeError(path, expected, str).fail
      }
    }

  def email(path: JPath) =
    regex("^[^@]+@[^@]+$".r, path, "notEmail")

  def sv: StringValidation =
    this
}
