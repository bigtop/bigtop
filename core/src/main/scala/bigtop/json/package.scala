package bigtop

import scalaz._
import scalaz.Scalaz._

package object json {

  type JsonValidation[T] = Validation[JsonErrors, T]

}