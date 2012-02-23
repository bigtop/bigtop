package bigtop
package json

import bigtop.util.{Uuid, Iso8601Format}
import bigtop.problem._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import org.joda.time.DateTime

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {
  import Problems._

  def mandatory[T](name: String)(implicit builder: JValue => Validation[Problem,T]): Validation[Problem,T] =
    (json \? name).toSuccess(Client.missingArgument(name)).flatMap(builder)

  def optional[T](name: String)(implicit builder: JValue => Validation[Problem,T]): Validation[Problem,Option[T]] =
    (json \? name) match {
      case Some(json) => builder(json).map(Some(_))
      case None       => (None : Option[T]).success[Problem]
    }

  def optional[T](name: String, default: T)(implicit builder: JValue => Validation[Problem,T]): Validation[Problem,T] =
    (json \? name).map(builder).getOrElse(default.success)
}

trait JsonImplicits {
  import Problems._

  implicit def jvalueToJValueW(json: JValue): JValueW = JValueW(json)

  implicit def buildString(json: JValue): Validation[Problem,String] =
    json -->? classOf[JString] map (_.value) toSuccess (malformed("string", json))

  implicit def buildBoolean(json: JValue): Validation[Problem,Boolean] =
    json -->? classOf[JBool] map (_.value) toSuccess (malformed("boolean", json))

  implicit def buildUuid(json: JValue): Validation[Problem,Uuid] =
    json -->? classOf[JString] map (_.value) flatMap (Uuid.parse _) toSuccess (malformed("uuid", json))

  implicit def buildIntr(json: JValue): Validation[Problem,Int] =
    json -->? classOf[JInt] map (_.value.intValue) toSuccess (malformed("int", json))

  implicit def buildDouble(json: JValue): Validation[Problem,Double] =
    json -->? classOf[JDouble] map (_.value) toSuccess (malformed("double", json))

  implicit def buildDateTime(json: JValue): Validation[Problem,DateTime] =
    json -->? classOf[JString] map (_.value) toSuccess (malformed("time", json)) flatMap (Iso8601Format.read(_)) match {
      case Failure(msg) => (malformed("time", json)).fail
      case Success(s)   => s.success
    }

  implicit def buildArray[A](builder: JValue => A)(json: JValue): Validation[Problem,Seq[A]] =
    json -->? classOf[JArray] map (_.elements.map(builder)) toSuccess (malformed("array", json))

  private def malformed(`type`: String, json: JValue) = {
    import blueeyes.json.Printer._
    Client.malformedArgument("data", "expected %s, found %s".format(`type`, compact(render(json))))
  }
}

object JsonImplicits extends JsonImplicits
