package bigtop
package json

import bigtop.concurrent._
import bigtop.problem._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

import JsonErrors.TypeError.{ apply => typeError }
import JsonErrors.Missing.{ apply => missing }

trait JValueImplicits {
  implicit def jsonToJValueW(in: JValue): JValueW = JValueW(in)
}

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {
  import JsonFormatters._

  def as[T](implicit reader: JsonReader[T]): JsonValidation[T] =
    reader.read(json)

  def asSeq[T](implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
    for {
      arr <- (json -->? classOf[JArray]).toSuccess(typeError("array", json))
      ans <- arr.elements.map(reader.read _).sequence[JsonValidation, T]
    } yield ans

  def asMap[T](implicit reader: JsonReader[T]): JsonValidation[Map[String,T]] =
    for {
      obj <- (json -->? classOf[JObject]).toSuccess(typeError("array", json))
      ans <- obj.fields.map { case JField(name, value) =>
               reader.read(value).map(value => name -> value)
             }.sequence[JsonValidation, (String, T)]
    } yield Map(ans : _*)

  def mandatory[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[T] =
    (json get path) match {
      case JNothing => JsonErrors.Missing(path).fail[T]
      case other    => prefixErrors(path, reader.read(other))
    }

  def optional[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[T]] =
    (json get path) match {
      case JNothing => Option.empty[T].success[JsonErrors]
      case other    => prefixErrors(path, reader.read(other).map(Some(_)))
    }

  def optional[T](path: JPath, default: T)(implicit reader: JsonReader[T]): JsonValidation[T] =
    (json get path) match {
      case JNothing => default.success[JsonErrors]
      case other    => prefixErrors(path, reader.read(other))
    }

  // TODO: Work out if we NEED these. If we do, uncomment them and TEST THEM:

  def mandatorySeq[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
    mandatory[JValue](path).flatMap(field => prefixErrors(path, field.asSeq[T]))

  def mandatoryMap[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Map[String, T]] =
    mandatory[JValue](path).flatMap(field => prefixErrors(path, field.asMap[T]))

  def optionalSeq[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[Seq[T]]] =
    for {
      optField <- optional[JValue](path)
      optValue <- optField match {
                    case Some(field) => prefixErrors(path, field.asSeq[T].map(Some(_)))
                    case None        => Option.empty[Seq[T]].success[JsonErrors]
                  }
    } yield optValue

  def optionalSeq[T](path: JPath, default: Seq[T])(implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
    for {
      optField <- optional[JValue](path)
      optValue <- optField match {
                    case Some(field) => prefixErrors(path, field.asSeq[T])
                    case None        => default.success[JsonErrors]
                  }
    } yield optValue

  def optionalMap[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[Map[String,T]]] =
    for {
      optField <- optional[JValue](path)
      optValue <- optField match {
                    case Some(field) => prefixErrors(path, field.asMap[T].map(Some(_)))
                    case None        => Option.empty[Map[String, T]].success[JsonErrors]
                  }
    } yield optValue

  def optionalMap[T](path: JPath, default: Map[String, T])(implicit reader: JsonReader[T]): JsonValidation[Map[String, T]] =
    for {
      optField <- optional[JValue](path)
      optValue <- optField match {
                    case Some(field) => prefixErrors(path, field.asMap[T])
                    case None        => default.success[JsonErrors]
                  }
    } yield optValue
}
