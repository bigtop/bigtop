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

trait IntFormats {
  def maxValue(value: Int) = new Validator[Int] {
    def read(in: Int) =
      if(in <= value) {
        in.success[JsonErrors]
      } else {
        typeError("maximum value " + value, in).fail[Int]
      }
    def write(in: Int) = in
  }

  def minValue(value: Int) = new Validator[Int] {
    def read(in: Int) =
      if(in >= value) {
        in.success[JsonErrors]
      } else {
        typeError("minimum value " + value, in).fail[Int]
      }
    def write(in: Int) = in
  }
}
