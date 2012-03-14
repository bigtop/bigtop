package bigtop
package user

import org.specs2.mutable.Specification
import blueeyes.core.http._
import blueeyes.concurrent.test.FutureMatchers
import bigtop.util._

class SecurityCheckSpec extends Specification with ValidationMatchers {

  "SecurityCheck.or" should {

    "call the other check if it fails" in {

      val check1 = SecurityCheck.simpleCheck[String,SimpleUser](_ => false, "dummy.1")
      val check2 = SecurityCheck.simpleCheck[String,SimpleUser](_ => true, "dummy.2")
      val check3 = check1 or check2

      val request = HttpRequest[String](HttpMethods.GET, URI("http://example.com/"))
      val user = SimpleUser(Uuid.create, "noel", Password.fromPassword("password"),
                            "Noel", "Welsh", false)

      check1(request, Some(user)).await must beFailure()
      check2(request, Some(user)).await must beSuccess(beSome(user))
      check3(request, Some(user)).await must beSuccess(beSome(user))
    }

  }

}
