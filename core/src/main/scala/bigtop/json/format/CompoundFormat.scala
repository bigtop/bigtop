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

trait CompoundFormats {
  implicit def optionFormat[A](implicit format: JsonFormat[A]): JsonFormat[Option[A]] =
    new JsonFormat[Option[A]] {
      def write(in: Option[A]) =
        in.map(format.write _).getOrElse(JNull)

      def read(in: JValue) =
        in match {
          case JNull => Option.empty[A].success[JsonErrors]
          case other => format.read(other).map(Some(_))
        }
    }

  implicit def seqFormat[A](implicit format: JsonFormat[A]): JsonFormat[Seq[A]] =
    new JsonFormat[Seq[A]] {
      def write(in: Seq[A]) =
        JArray(in.map(format.write _).toList)

      def read(json: JValue) =
        (json -->? classOf[JArray]).
        toSuccess(typeError("array", json)).
        flatMap(_.elements.map(format.read _).sequence[JsonValidation, A])
    }

  implicit def mapFormat[A, B](implicit keyFormat: Format[JsonErrors, A, String], valFormat: JsonFormat[B]): JsonFormat[Map[A,B]] =
    new JsonFormat[Map[A,B]] {
      def write(in: Map[A,B]) =
        JObject(in.toIterable.foldLeft(Nil: List[JField]){
          (accum, pair) => JField(keyFormat.write(pair._1), valFormat.write(pair._2)) :: accum
        })

      def read(json: JValue) =
        (json -->? classOf[JObject]).
        toSuccess(typeError("object", json)).
        flatMap { obj =>
          for {
            fields <- obj.fields.foldLeft(Nil: List[JsonValidation[(A, B)]]) { (accum, field) =>
                        (for {
                          key   <- keyFormat.read(field.name)
                          value <- valFormat.read(field.value)
                        } yield (key -> value)) :: accum
                      }.sequence[JsonValidation, (A, B)]
          } yield Map[A,B]() ++ fields
        }
    }

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
