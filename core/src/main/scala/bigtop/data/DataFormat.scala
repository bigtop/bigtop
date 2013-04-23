package bigtop.data

import bigtop.json._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz._
import scalaz.Scalaz._

trait DataFormat[T, S] extends Format[JsonErrors, T, S] with DataReader[T, S] with DataWriter[T, S] {
  self =>

  def andThen[U](inner: DataFormat[U, T]): DataFormat[U, S] =
    new DataFormat[U, S] {
      def read(in: S) = self.read(in).flatMap(inner.read _)
      def write(in: U) = self.write(inner.write(in))
    }

  def lift[M[_] : Traverse : Monad]: DataFormat[M[T], M[S]] =
    new DataFormat[M[T], M[S]] {
      val monad = implicitly[Monad[M]]
      val traverse = implicitly[Traverse[M]]
      val applicative = implicitly[Applicative[JsonValidation]]
      def read(in: M[S]) = applicative.traverse[S, M, T](in)(self.read _)
      def write(in: M[T]) = monad.map(in)(self.write _)
    }
}
