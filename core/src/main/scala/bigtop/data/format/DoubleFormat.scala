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

trait DoubleFormats {
  def maxValue(value: Double) = new Validator[Double] {
    def read(in: Double) =
      if(in <= value) {
        in.success[JsonErrors]
      } else {
        typeError("maximum value " + value, in).fail[Double]
      }
    def write(in: Double) = in
  }

  def minValue(value: Double) = new Validator[Double] {
    def read(in: Double) =
      if(in >= value) {
        in.success[JsonErrors]
      } else {
        typeError("minimum value " + value, in).fail[Double]
      }
    def write(in: Double) = in
  }
}
