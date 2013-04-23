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

trait AnyValidators {
  def identity[T] = new Validator[T] {
    def read(in: T) = in.success[JsonErrors]
    def write(in: T) = in
  }
}