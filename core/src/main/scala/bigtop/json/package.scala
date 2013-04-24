package bigtop

import bigtop.data._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

package object json {

  type JsonValidation[T] = Validation[JsonErrors, T]

  type JsonFormat[T] = DataFormat[T, JValue]

  type JsonReader[T] = DataReader[T, JValue]

  type JsonWriter[T] = DataWriter[T, JValue]

}