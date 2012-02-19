package bigtop
package http

import bigtop.util._
import bigtop.problem._
import blueeyes.core.http._
import scalaz._
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import org.joda.time.DateTime

/** A "wide" (i.e. pimped) JSON value */
case class HttpRequestW[Content](request: HttpRequest[Content]) {
  import Problems._

  def mandatoryParam[T](name: Symbol)(implicit builder: String => Validation[Problem,T]): Validation[Problem,T] =
    request.parameters.get(name).toSuccess(Client.missingArgument(name.name)).flatMap(builder)

  def optionalParam[T](name: Symbol)(implicit builder: String => Validation[Problem,T]): Validation[Problem,Option[T]] =
    request.parameters.get(name) match {
      case Some(str) => builder(str).map(Some(_))
      case None      => (None : Option[T]).success[Problem]
    }

  def optionalParam[T](name: Symbol, default: T)(implicit builder: String => Validation[Problem,T]): Validation[Problem,T] =
    request.parameters.get(name).map(builder).getOrElse(default.success)
}

trait RequestParameterImplicits {
  import Problems._

  implicit def httpRequestToHttp[T](req: HttpRequest[T]): HttpRequestW[T] = HttpRequestW(req)

  implicit def buildString(str: String): Validation[Problem,String] =
    str.success[Problem]

  implicit def buildUuid(str: String): Validation[Problem,Uuid] =
    Uuid.parse(str).toSuccess(malformed("uuid", str))

  implicit def buildInt(str: String): Validation[Problem,Int] =
    parseInt(str).toSuccess(malformed("int", str))

  implicit def buildDouble(str: String): Validation[Problem,Double] =
    parseDouble(str).toSuccess(malformed("double", str))

  implicit def buildDateTime(str: String): Validation[Problem,DateTime] =
    Iso8601Format.read(str).bimap(
      f = str => malformed("ISO8601 date-time (%s)".format(Iso8601Format.millisFormatString), str),
      g = dt => dt
    )

  private def malformed(`type`: String, str: String) =
    Client.malformedArgument("data", "expected %s, found '%s'".format(`type`, str))

  private def parseInt(str: String) =
    try {
      Some(str.toInt)
    } catch {
      case exn: NumberFormatException => None
    }

  private def parseDouble(str: String) =
    try {
      Some(str.toDouble)
    } catch {
      case exn: NumberFormatException => None
    }
}

object RequestParameterImplicits extends RequestParameterImplicits
