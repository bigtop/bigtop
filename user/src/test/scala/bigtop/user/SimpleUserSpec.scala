package bigtop
package user

import org.specs2.mutable.Specification
import bigtop.util._

class SimpleUserSpec extends Specification {

  val topSecret = Password.fromPassword("topsecret")

  "Password" should {

    "be hashed when generated from password" in {
      BCrypt.isHash(Password.fromPassword("topsecret").hash)
    }

    "not be rehashed when generated from hash" in {
      Password.fromHash(topSecret.hash) mustEqual topSecret
    }
  }

  "SimpleUser.isPasswordOk" should {

    "return true when the password is correct" in {
      SimpleUser(Uuid.create, "noel", topSecret, "Noel", "Welsh", true).isPasswordOk("topsecret")
    }

    "return false when the password is incorrect" in {
      !SimpleUser(Uuid.create, "noel", topSecret, "Noel", "Welsh", false).isPasswordOk("far00bar")
    }
  }

}
