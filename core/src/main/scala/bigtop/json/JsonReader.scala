package bigtop.json

import bigtop.data._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz.{ Reader => ScalazReader, _ }
import scalaz.Scalaz._

object JsonReader {
  def apply[T](fn: JValue => JsonValidation[T]) =
    new JsonReader[T] {
      def read(in: JValue) = fn(in)
    }
}

trait JsonReader[T] extends DataReader[T, JValue] {
  self =>

  override def map[U](fn: T => U): JsonReader[U] =
    new JsonReader[U] {
      def read(in: JValue) = self.read(in).map(fn)
    }

  override def flatMap[U](fn: T => JsonValidation[U]): JsonReader[U] =
    new JsonReader[U] {
      def read(in: JValue) = self.read(in).flatMap(fn)
    }

  override def andThen[U](inner: DataReader[U, T]): JsonReader[U] =
    new JsonReader[U] {
      def read(in: JValue) = self.read(in).flatMap(inner.read _)
    }

  // override def lift[M[_] : Traverse : Monad]: JsonReader[M[T]] =
  //   new JsonReader[M[T]] {
  //     val monad = implicitly[Monad[M]]
  //     val traverse = implicitly[Traverse[M]]
  //     val applicative = implicitly[Applicative[JsonValidation]]
  //     def read(in: JValue) = applicative.traverse[JValue, M, T](monad.point(in))(self.read _)
  //   }
}
