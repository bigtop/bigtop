package bigtop
package json

import blueeyes.json.JsonAST._
import scalaz.{Validation, ValidationNEL}
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._


/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {

  def / [E,T](name: String, msg: E)(implicit builder: JValue => Option[T]): Validation[E,T] =
    builder(json \ name) toSuccess(msg)

  def /? [E,T](name: String, default: T, msg: E)(implicit builder: JValue => Option[T]): Validation[E,T] =
    (json \? name).map(builder(_) toSuccess(msg)).getOrElse(default.success)
}

object JsonImplicits {
  implicit def jvalueToJValueW(json: JValue): JValueW = JValueW(json)

  implicit def buildString(json: JValue): Option[String] =
      json -->? classOf[JString] map (_.value)
  implicit def buildInteger(json: JValue): Option[Int] =
      json -->? classOf[JInt] map (_.value.intValue)
  implicit def buildDouble(json: JValue): Option[Double] =
      json -->? classOf[JDouble] map (_.value)
  implicit def buildArray[A](builder: JValue => A)(json: JValue): Option[Seq[A]] =
      json -->? classOf[JArray] map { _.elements.map(builder) }
}
