package bigtop.http

import bigtop.json._
import bigtop.util._
import bigtop.problem._
import blueeyes.core.http._
import blueeyes.json.JPath
import scalaz._
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import org.joda.time.DateTime

/** A "wide" (i.e. pimped) HttpRequest value */
trait HttpRequestW[Content] {
  import RequestParameterImplicits._

  val request: HttpRequest[Content]

  def mandatoryParam[T](name: Symbol)(implicit builder: String => JsonValidation[T]): JsonValidation[T] =
    request.parameters.get(name) match {
      case None      => JsonErrors.Missing(name.name).fail[T]
      case Some(str) => prefixErrors(name.name, builder(str))
    }

  def optionalParam[T](name: Symbol)(implicit builder: String => JsonValidation[T]): JsonValidation[Option[T]] =
    request.parameters.get(name) match {
      case None      => Option.empty[T].success[JsonErrors]
      case Some(str) => prefixErrors(name.name, builder(str).map(Some(_)))
    }

  def optionalParam[T](name: Symbol, default: T)(implicit builder: String => JsonValidation[T]): JsonValidation[T] =
    request.parameters.get(name) match {
      case None      => default.success[JsonErrors]
      case Some(str) => prefixErrors(name.name, builder(str))
    }
}

object RequestParameterImplicits extends RequestParameterImplicits

trait RequestParameterImplicits extends JsonValidationCombinators {
  implicit def httpRequestToHttpRequestW[T](req: HttpRequest[T]): HttpRequestW[T] =
    new HttpRequestW[T] {
      val request = req
    }

  implicit def buildString(str: String): JsonValidation[String] =
    str.success[JsonErrors]

  implicit def buildUuid(str: String): JsonValidation[Uuid] =
    Uuid.parse(str).toSuccess(malformed("uuid", str))

  implicit def buildBoolean(str: String): JsonValidation[Boolean] =
    str.toLowerCase match {
      case "true"  => true.success[JsonErrors]
      case "false" => false.success[JsonErrors]
      case _       => malformed("boolean", str).fail[Boolean]
    }

  implicit def buildInt(str: String): JsonValidation[Int] =
    parseInt(str).toSuccess(malformed("int", str))

  implicit def buildDouble(str: String): JsonValidation[Double] =
    parseDouble(str).toSuccess(malformed("double", str))

  implicit def buildDateTime(str: String): JsonValidation[DateTime] =
    Iso8601Format.read(str).bimap(
      f = str => malformed("ISO8601 date-time (%s)".format(Iso8601Format.millisFormatString), str),
      g = dt => dt
    )

  def parseInt(str: String) =
    try { Some(str.toInt) } catch { case exn: NumberFormatException => None }

  def parseDouble(str: String) =
    try { Some(str.toDouble) } catch { case exn: NumberFormatException => None }
}
