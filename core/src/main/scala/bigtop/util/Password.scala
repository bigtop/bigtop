package bigtop
package util

import bigtop.problem._
import scalaz._
import scalaz.Scalaz._

case class Password private (val hash: String) {
  def matches(password: String): Boolean = {
    BCrypt.compare(password, hash)
  }
}

object Password {
  def fromPassword(password: String): Password = {
    Password(BCrypt.hash(password))
  }

  def fromHash(hash: String): Password = {
    Password(hash)
  }

  def parseHash(hash: String): Validation[Problem, Password] = {
    if(BCrypt.isHash(hash)) {
      Password(hash).success[Problem]
    } else {
      Problems.Malformed("bcryptHash", "not.a.hash").fail[Password]
    }
  }
}
