package bigtop.data

import bigtop.json._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz.{ Reader => ScalazReader, _ }
import scalaz.Scalaz._

object DataReader {
  def apply[T, S](fn: S => JsonValidation[T]) =
    new DataReader[T, S] {
      def read(in: S) = fn(in)
    }
}

trait DataReader[T, S] extends Reader[JsonErrors, T, S] {
  self =>

  def map[U](fn: T => U): DataReader[U, S] =
    new DataReader[U, S] {
      def read(in: S) = self.read(in).map(fn)
    }

  def flatMap[U](fn: T => JsonValidation[U]): DataReader[U, S] =
    new DataReader[U, S] {
      def read(in: S) = self.read(in).flatMap(fn)
    }

  def andThen[U](inner: DataReader[U, T]): DataReader[U, S] =
    new DataReader[U, S] {
      def read(in: S) = self.read(in).flatMap(inner.read _)
    }

  def lift[M[_] : Traverse]: DataReader[M[T], M[S]] =
    new DataReader[M[T], M[S]] {
      val applicative = implicitly[Applicative[JsonValidation]]
      val traverse = implicitly[Traverse[M]]
      def read(in: M[S]) = applicative.traverse[S, M, T](in)(self.read _)
    }
}
