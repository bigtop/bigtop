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

trait TupleFormats {
  def tupleFormat[A, B](a : JsonFormat[A], b : JsonFormat[B]): JsonFormat[(A, B)] =
    new JsonFormat[(A, B)] {
      def write(in: (A, B)) =
        a.write(in._1) merge b.write(in._2)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in)).tupled
    }

  def tupleFormat[A, B, C](a: JsonFormat[A], b: JsonFormat[B], c: JsonFormat[C]): JsonFormat[(A, B, C)] =
    new JsonFormat[(A, B, C)] {
      def write(in: (A, B, C)) =
        a.write(in._1) merge b.write(in._2) merge c.write(in._3)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in) |@| c.read(in)).tupled
    }

  def tupleFormat[A, B, C, D](a: JsonFormat[A], b: JsonFormat[B], c: JsonFormat[C], d: JsonFormat[D]): JsonFormat[(A, B, C, D)] =
    new JsonFormat[(A, B, C, D)] {
      def write(in: (A, B, C, D)) =
        a.write(in._1) merge b.write(in._2) merge c.write(in._3) merge d.write(in._4)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in)).tupled
    }

  def tupleFormat[A, B, C, D, E](a: JsonFormat[A], b: JsonFormat[B], c: JsonFormat[C], d: JsonFormat[D], e: JsonFormat[E]): JsonFormat[(A, B, C, D, E)] =
    new JsonFormat[(A, B, C, D, E)] {
      def write(in: (A, B, C, D, E)) =
        a.write(in._1) merge b.write(in._2) merge c.write(in._3) merge d.write(in._4) merge e.write(in._5)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in)).tupled
    }

  def tupleFormat[A, B, C, D, E, F](a: JsonFormat[A], b: JsonFormat[B], c: JsonFormat[C], d: JsonFormat[D], e: JsonFormat[E], f: JsonFormat[F]): JsonFormat[(A, B, C, D, E, F)] =
    new JsonFormat[(A, B, C, D, E, F)] {
      def write(in: (A, B, C, D, E, F)) =
        a.write(in._1) merge b.write(in._2) merge c.write(in._3) merge d.write(in._4) merge e.write(in._5) merge f.write(in._6)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in) |@| f.read(in)).tupled
    }

  def tupleFormat[A, B, C, D, E, F, G](a: JsonFormat[A], b: JsonFormat[B], c: JsonFormat[C], d: JsonFormat[D], e: JsonFormat[E], f: JsonFormat[F], g: JsonFormat[G]): JsonFormat[(A, B, C, D, E, F, G)] =
    new JsonFormat[(A, B, C, D, E, F, G)] {
      def write(in: (A, B, C, D, E, F, G)) =
        a.write(in._1) merge b.write(in._2) merge c.write(in._3) merge d.write(in._4) merge e.write(in._5) merge f.write(in._6) merge g.write(in._7)

      def read(in: JValue) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in) |@| f.read(in) |@| g.read(in)).tupled
    }
}
