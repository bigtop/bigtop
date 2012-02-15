package bigtop

import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.user.User

package object session {

  trait SessionTypes[U <: User] {

    type SessionValidation = FutureValidation[Problem, Session[U]]
    type UnitValidation = FutureValidation[Problem, Unit]

  }

}
