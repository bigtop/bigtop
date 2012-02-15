package bigtop
package util

import org.specs2.mutable.Specification

class BCryptSpec extends Specification {

  "BCrypt.isHash" should {

    "return true for BCrypt hashes" in {
      BCrypt.isHash(BCrypt.hash("foobar"))
      BCrypt.isHash(BCrypt.hash("1234567890"))
      BCrypt.isHash(BCrypt.hash("adb-45345-dfgfd"))
    }

    "return false for other strings" in {
      !BCrypt.isHash("foobar")
      !BCrypt.isHash("1234567890")
      !BCrypt.isHash("abd-45345-dfgfd")
    }

  }

}
