package bigtop
package util

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._

class EmailSpec extends Specification {

  "Email.create()" should {
    "create a UUID that can be read by Email.parse" in {
      val uuid = Email.create()

      Email.parse(uuid.toString) must beSome(uuid)
    }
  }

  "Email.parse" should {
    "succeed given valid string" in {
      Email.parse("a@b").isDefined mustEqual true
      Email.parse("A@B").isDefined mustEqual true
    }

    "fail given invalid string" in {
      Email.parse("a") mustEqual None
      Email.parse("@") mustEqual None
      Email.parse("a@") mustEqual None
      Email.parse("@b") mustEqual None
      Email.parse("a@@b") mustEqual None
    }
  }
}
