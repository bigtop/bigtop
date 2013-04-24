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


trait NullableFormats {
  def nullable[A](implicit f: JsonFormat[A]): JsonFormat[Option[A]] =
    new NullableFormat[A] {
      val reader = f
      val writer = f
    }

  def nullable[A](orElse: => A)(implicit f: JsonFormat[A]): JsonFormat[A] =
    new DefaultFormat[A] {
      def default = orElse
      val reader = f
      val writer = f
    }
}

trait NullableReaders {
  def nullable[A](implicit r: JsonReader[A]): JsonReader[Option[A]] =
    new NullableReader[A] {
      val reader = r
    }

  def nullable[A](orElse: => A)(implicit r: JsonReader[A]): JsonReader[A] =
    new DefaultReader[A] {
      def default = orElse
      val reader = r
    }
}

trait NullableWriters {
  def nullable[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] =
    new NullableWriter[A] {
      val writer = w
    }

  def nullable[A](orElse: => A)(implicit w: JsonWriter[A]): JsonWriter[A] =
    new DefaultWriter[A] {
      def default = orElse
      val writer = w
    }
}

trait NullableReader[A] extends JsonReader[Option[A]] {
  def reader: JsonReader[A]
  def read(in: JValue) =
    in match {
      case JNull    => Option.empty[A].success[JsonErrors]
      case JNothing => Option.empty[A].success[JsonErrors]
      case other    => reader.read(other).map(Some(_))
    }
}

trait NullableWriter[A] extends JsonWriter[Option[A]] {
  def writer: JsonWriter[A]
  def write(in: Option[A]) = in.map(writer.write _).getOrElse(JNothing)
}

trait NullableFormat[A] extends NullableReader[A] with NullableWriter[A] with JsonFormat[Option[A]]

trait DefaultReader[A] extends JsonReader[A] {
  def default: A
  def reader: JsonReader[A]
  def read(in: JValue) =
    in match {
      case JNull    => default.success[JsonErrors]
      case JNothing => default.success[JsonErrors]
      case other    => reader.read(other)
    }
}

trait DefaultWriter[A] extends JsonWriter[A] {
  def default: A
  def writer: JsonWriter[A]
  def write(in: A) = writer.write(in)
}

trait DefaultFormat[A] extends DefaultReader[A] with DefaultWriter[A] with JsonFormat[A]
