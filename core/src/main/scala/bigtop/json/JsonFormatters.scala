package bigtop
package json

import bigtop.concurrent._
import bigtop.json.format._
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

object JsonFormatters extends JsonFormatters
  with JsonValidationCombinators
  with JPathImplicits
  with JValueImplicits
  with JsonWritableImplicits {

  val __ = JPath.Identity

  case class JPathW(path: JPath) {
    def as[T : JsonFormat] = pathFormat[T](path)
    def reader[T : JsonReader] = pathReader[T](path)
    def writer[T : JsonWriter] = pathWriter[T](path)
  }

  implicit def jPathTojPathW(in: JPath): JPathW = JPathW(in)
}

trait JsonFormatters extends BaseFormats with CompoundFormats with PathFormats
