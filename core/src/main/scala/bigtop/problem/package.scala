package bigtop

import scalaz._
import scalaz.Scalaz._

package object problem {

  type ProblemValidation[T] = Validation[Problem, T]

}