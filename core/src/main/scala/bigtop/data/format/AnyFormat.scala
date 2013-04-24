package bigtop.data
package format

import bigtop.json._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

import bigtop.json.JsonErrors.Missing.{ apply => missing }
import bigtop.json.JsonErrors.TypeError.{ apply => typeError }

trait AnyFormats {
  implicit def identityFormat[T]: DataFormat[T, T] =
    new DataFormat[T, T] {
      def read(in: T) =
        in.success[JsonErrors]

      def write(in: T) =
        in
    }

  implicit def identityReader[T]: DataReader[T, T] = identityFormat[T]
}