package bigtop
package json

import blueeyes.json.JsonAST._
import scalaz._
import Scalaz._

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {

  def / [E,T](name: String, msg: E)(implicit builder: JValue => Option[T]): ValidationNEL[E,T] =
    builder(json \ name) toSuccess(msg) liftFailNel

  def /? [E,T](name: String, default: T, msg: E)(implicit builder: JValue => Option[T]): ValidationNEL[E,T] =
    (json \? name).map(builder(_) toSuccess(msg) liftFailNel).getOrElse(default.success)
}

object Implicits {
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
