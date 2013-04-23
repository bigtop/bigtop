package bigtop.json

import bigtop.data._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz._
import scalaz.Scalaz._

trait JsonFormat[T] extends DataFormat[T, JValue] with JsonReader[T] with JsonWriter[T] {
  self =>

  override def andThen[U](inner: DataFormat[U, T]): JsonFormat[U] =
    new JsonFormat[U] {
      def read(in: JValue) = self.read(in).flatMap(inner.read)
      def write(in: U) = self.write(inner.write(in))
    }

  // override def lift[M[_] : Traverse : Monad]: JsonFormat[M[T]] =
  //   new JsonFormat[M[T]] {
  //     val monad = implicitly[Monad[M]]
  //     val traverse = implicitly[Traverse[M]]
  //     val applicative = implicitly[Applicative[JsonValidation]]
  //     def read(in: JValue) = applicative.traverse[JValue, M, T](monad.point(in))(self.read _)
  //     def write(in: M[T]) = monad.map(in)(self.write _)
  //   }
}
