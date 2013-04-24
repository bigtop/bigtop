package bigtop.json
package format

import bigtop.concurrent._
import bigtop.problem._
import bigtop.data.DataFormat
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

trait MapFormats {
  implicit def mapFormat[A, B](implicit k: DataFormat[A, String], v: JsonFormat[B]) =
    new MapFormat[A, B] {
      val keyFormat = k
      val valFormat = v
    }
}

trait MapReaders {
  implicit def mapReader[A, B](implicit k: DataReader[A, String], v: JsonReader[B]) =
    new MapReader[A, B] {
      val keyReader = k
      val valReader = v
    }
}

trait MapWriters {
  implicit def mapWriter[A, B](implicit k: DataWriter[A, String], v: JsonWriter[B]) =
    new MapWriter[A, B] {
      val keyWriter = k
      val valWriter = v
    }
}

trait MapFormat[A, B] extends MapReader[A, B] with MapWriter[A, B] with JsonFormat[Map[A, B]] {
  def keyFormat: DataFormat[A, String]
  def valFormat: JsonFormat[B]
  val keyReader = keyFormat
  val valReader = valFormat
  val keyWriter = keyFormat
  val valWriter = valFormat
}

trait MapReader[A, B] extends JsonReader[Map[A, B]] {
  def keyReader: DataReader[A, String]
  def valReader: JsonReader[B]
  def read(json: JValue) =
    (json -->? classOf[JObject]).
    toSuccess(typeError("object", json)).
    flatMap { obj =>
      for {
        fields <- obj.fields.foldLeft(Nil: List[JsonValidation[(A, B)]]) { (accum, field) =>
                    (for {
                      key   <- keyReader.read(field.name)
                      value <- valReader.read(field.value)
                    } yield (key -> value)) :: accum
                  }.sequence[JsonValidation, (A, B)]
      } yield Map[A,B]() ++ fields
    }
}

trait MapWriter[A, B]  extends JsonWriter[Map[A, B]] {
  def keyWriter: DataWriter[A, String]
  def valWriter: JsonWriter[B]
  def write(in: Map[A, B]) =
    JObject(in.toIterable.foldLeft(Nil: List[JField]){
      (accum, pair) =>
        JField(
          keyWriter.write(pair._1),
          valWriter.write(pair._2)
        ) :: accum
    })
}
