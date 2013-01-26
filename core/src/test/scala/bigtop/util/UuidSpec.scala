package bigtop
package util

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._

class UuidSpec extends Specification {

  "Uuid.create()" should {
    "create a UUID that can be read by Uuid.parse" in {
      val uuid = Uuid.create

      Uuid.parse(uuid.toString) must beSome(uuid)
    }
  }

  "Uuid.parse" should {
    "succeed given valid string" in {
      Uuid.parse("aa6622d0-edc7-11e0-9d62-005056c00008").isDefined mustEqual true
      Uuid.parse("AA6622D0-EDC7-11E0-9D62-005056C00008").isDefined mustEqual true
    }

    "fail given invalid string" in {
      Uuid.parse("bad uuid") mustEqual None
    }
  }
}
