package bigtop.json
package format

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

trait SequenceFormats {
  implicit def seqFormat[A](implicit inner: JsonFormat[A]) =
    new SeqFormat[A] {
      val format = inner
    }
}

trait SequenceReaders {
  implicit def seqReader[A](implicit inner: JsonReader[A]) =
    new SeqReader[A] {
      val reader = inner
    }
}

trait SequenceWriters {
  implicit def seqWriter[A](implicit inner: JsonWriter[A]) =
    new SeqWriter[A] {
      val writer = inner
    }
}

trait SeqFormat[A] extends SeqReader[A] with SeqWriter[A] with JsonReader[A] {
  def format: JsonFormat[A]
  val reader = format
  val writer = format
}

trait SeqReader[A] extends JsonReader[A] {
  def reader: JsonReader[A]
  def read(json: JValue) =
    (json -->? classOf[JArray]).
    toSuccess(typeError("array", json)).
    flatMap(_.elements.map(reader.read _).sequence[JsonValidation, A])
}

trait SeqWriter[A] extends JsonWriter[A] {
  def writer: JsonWriter[A]
  def write(in: Seq[A]) =
    JArray(in.map(writer.write _).toList)
}
