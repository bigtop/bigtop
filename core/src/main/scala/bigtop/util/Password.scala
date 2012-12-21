package bigtop.util

import bigtop.json._
import bigtop.problem._
import blueeyes.json.JPath
import scalaz._
import scalaz.Scalaz._

case class Password private (val hash: String) {
  def matches(password: String): Boolean =
    BCrypt.compare(password, hash)
}

object Password {
  def fromPassword(password: String): Password =
    Password(BCrypt.hash(password))

  def fromHash(hash: String): Password =
    Password(hash)

  def parseHash(hash: String): JsonValidation[Password] =
    if(BCrypt.isHash(hash)) {
      Password(hash).success[JsonErrors]
    } else {
      JsonErrors.Malformed(JPath.Identity, "Not a password hash.").fail[Password]
    }
}
