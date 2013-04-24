package bigtop.data

import bigtop.json._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz._
import scalaz.Scalaz._

object DataFormats extends DataFormats with ParamImplicits

trait DataFormats extends format.AnyFormats
  with format.StringFormats
  with format.IntFormats
  with format.DoubleFormats
  with format.OptionFormats
  with format.PrefixFormats
  with format.ParamFormats
  with format.TupleFormats

object DataFormat {
  def apply[A, B](reader: A => JsonValidation[B], writer: B => A): DataFormat[B, A] =
    new DataFormat[B, A] {
      def read(in: A) = reader(in)
      def write(in: B) = writer(in)
    }

  def transform[T](reader: T => T = (in: T) => in, writer: T => T = (in: T) => in): DataFormat[T, T] =
    new DataFormat[T, T] {
      def read(in: T) = reader(in).success[JsonErrors]
      def write(in: T) = in
    }

  def validate[T](reader: T => JsonValidation[T] = (in: T) => in.success[JsonErrors], writer: T => T = (in: T) => in): DataFormat[T, T] =
    new DataFormat[T, T] {
      def read(in: T) = reader(in)
      def write(in: T) = writer(in)
    }

  def identity[T]: DataFormat[T, T] =
    new DataFormat[T, T] {
      def read(in: T) = in.success[JsonErrors]
      def write(in: T) = in
    }
}

trait DataFormat[T, S] extends Format[JsonErrors, T, S] with DataReader[T, S] with DataWriter[T, S] {
  self =>

  def andThen[U](inner: DataFormat[U, T]): DataFormat[U, S] =
    new DataFormat[U, S] {
      def read(in: S) = self.read(in).flatMap(inner.read _)
      def write(in: U) = self.write(inner.write(in))
    }

  def apply[U](reader: T => U, writer: U => T): DataFormat[U, S] =
    new DataFormat[U, S] {
      def read(in: S) = self.read(in).map(reader)
      def write(in: U) = self.write(writer(in))
    }

  def andThen[U](reader: T => JsonValidation[U], writer: U => T): DataFormat[U, S] =
    new DataFormat[U, S] {
      def read(in: S) = self.read(in).flatMap(reader)
      def write(in: U) = self.write(writer(in))
    }

  // def liftFormat[M[_] : Traverse : Monad]: DataFormat[M[T], M[S]] =
  //   new DataFormat[M[T], M[S]] {
  //     val monad = implicitly[Monad[M]]
  //     val traverse = implicitly[Traverse[M]]
  //     val applicative = implicitly[Applicative[JsonValidation]]
  //     def read(in: M[S]) = applicative.traverse[S, M, T](in)(self.read _)
  //     def write(in: M[T]) = monad.map(in)(self.write _)
  //   }
}
