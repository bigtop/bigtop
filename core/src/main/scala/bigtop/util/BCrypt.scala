package bigtop
package util

import org.mindrot.jbcrypt.{BCrypt => JBCrypt}

/** Helpers for using jBCrypt. */
object BCrypt {

  /** Length of a BCrypt hashed password, in characters */
  val hashedLength = 60

  /**
   * (Log) number of founds to use when creating salt.
   */
  val rounds = 10

  /** Compare a plain text password to hashed one, returning true if they are equal */
  def compare(plainText: String, hash: String): Boolean = JBCrypt.checkpw(plainText, hash)

  /** Hash a plain text password, using the value of rounds to generate salt */
  def hash(plainText: String) = JBCrypt.hashpw(plainText, JBCrypt.gensalt(rounds))

  /** Hash a plain text password, given salt */
  def hash(plainText: String, salt: String) = JBCrypt.hashpw(plainText, salt)

  /** Create salt using the value of rounds */
  def createSalt() = JBCrypt.gensalt(rounds)

  /** Create salt using the given number of rounds */
  def createSalt(rounds: Int) = JBCrypt.gensalt(rounds)

  /** True if the string looks like a BCrypt hash */
  def isHash(str: String): Boolean =
    str.length == hashedLength &&
    HashRegex.findFirstIn(str).isDefined

  // Internal stuff --------------------------------------------------

  val HashRegex = "^\\$2a\\$[0-9]{2}\\$".r

}
