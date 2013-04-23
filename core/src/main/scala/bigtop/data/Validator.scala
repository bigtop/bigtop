package bigtop.data

import bigtop.data.format._
import bigtop.json._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

// trait Validator[T] extends DataFormat[T, T] {
//   self =>

//   def write(in: T) = in

//   def andThen(inner: DataFormat[T, T]): Validator[T] =
//     new Validator[T] {
//       def read(in: T) = self.read(in).flatMap(inner.read(_))
//       override def write(in: T) = self.write(inner.write(in))
//     }

//   override def andThen[U](inner: DataFormat[U, T]): DataFormat[U, T] =
//     new DataFormat[U, T] {
//       def read(in: T) = self.read(in).flatMap(inner.read(_))
//       def write(in: U) = self.write(inner.write(in))
//     }
// }

object Validator {
  def apply[T](fn: T => JsonValidation[T]) =
    new Validator[T] {
      def read(in: T) = fn(in)
      def write(in: T) = in
    }
}

object Validators extends AnyValidators
  with StringValidators
  with IntValidators
  with DoubleValidators