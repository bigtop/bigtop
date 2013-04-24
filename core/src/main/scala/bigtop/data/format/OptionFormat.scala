package bigtop.data
package format

import bigtop.json._
import scalaz._
import scalaz.Scalaz._

import bigtop.json.JsonErrors.Missing.{ apply => missing }
import bigtop.json.JsonErrors.TypeError.{ apply => typeError }

trait OptionFormats {
  def optional[S, T](inner: DataFormat[T, S]): DataFormat[Option[T], Option[S]] =
    new DataFormat[Option[T], Option[S]] {
      def read(in: Option[S]) =
        in match {
          case Some(in) => inner.read(in).map(Some(_))
          case None     => Option.empty[T].success[JsonErrors]
        }

      def write(in: Option[T]) =
        in.map(inner.write _)
    }

  def optional[S, T](inner: DataReader[T, S]): DataReader[Option[T], Option[S]] =
    new DataReader[Option[T], Option[S]] {
      def read(in: Option[S]) =
        in match {
          case Some(in) => inner.read(in).map(Some(_))
          case None     => Option.empty[T].success[JsonErrors]
        }
    }

  def default[T](orElse: => T): DataFormat[T, Option[T]] =
    new DataFormat[T, Option[T]] {
      def read(in: Option[T]) =
        in.getOrElse(orElse).success[JsonErrors]

      def write(in: T) =
        Some(in)
    }

  def required[T]: DataFormat[T, Option[T]] =
    new DataFormat[T, Option[T]] {
      def read(in: Option[T]) =
        in.toSuccess(missing(""))

      def write(in: T) =
        Some(in)
    }
}