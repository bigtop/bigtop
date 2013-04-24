package bigtop

import blueeyes.core.http._

package object data {

  type Validator[T] = DataFormat[T, T]

  type StringFormat[T] = DataFormat[T, String]

  type StringReader[T] = DataReader[T, String]

}