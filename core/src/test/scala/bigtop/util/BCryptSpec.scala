package bigtop
package util

import org.specs2.mutable.Specification

class BCryptSpec extends Specification {

  "BCrypt.isHash" should {

    "return true for BCrypt hashes" in {
      BCrypt.isHash(BCrypt.hash("foobar")) mustEqual true
      BCrypt.isHash(BCrypt.hash("1234567890")) mustEqual true
      BCrypt.isHash(BCrypt.hash("adb-45345-dfgfd")) mustEqual true
    }

    "return false for other strings" in {
      BCrypt.isHash("foobar") mustEqual false
      BCrypt.isHash("1234567890") mustEqual false
      BCrypt.isHash("abd-45345-dfgfd") mustEqual false
    }

  }

}
