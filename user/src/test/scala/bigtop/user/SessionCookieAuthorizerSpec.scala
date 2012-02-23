package bigtop
package user

import org.specs2.mutable.Specification
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._

class SessionCookieAuthorizerSpec extends Specification {

  "SessionCookie" should {

    "return some cookie if one exists" in {
      val request =  HttpRequest[ByteChunk](
        HttpMethods.GET,
        URI("http//example.com/"),
        headers = HttpHeaders.Empty + Cookie(CookiesPattern("session=1234"))
      )

      val cookie  = SessionCookie.get(request)

      cookie must beLike {
        case Some(cookie) =>
          cookie.name mustEqual "session"
          cookie.cookieValue mustEqual "1234"
      }
    }

  }

}
