package bigtop
package user

import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import bigtop.util.Uuid
import bigtop.concurrent._
import bigtop.problem._
import bigtop.problem.Problems._
import scalaz.Validation
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

trait SessionCookieAuthenticator[U <: User] extends Authenticated[U]
    with HttpHeaderImplicits
    with FutureImplicits
{

  def action: SessionRead[U]

  val cookieName = "session"

  def sessionCookie[A](request: HttpRequest[A]): Option[HttpCookie] = {
    val cookies = request.headers.header[Cookie].flatMap(_.cookies.headOption)
    cookies.filter(_.name == cookieName).headOption
  }

  def authenticated[A](request: HttpRequest[A]): FutureValidation[Problem,Option[U]] = {
    sessionCookie(request) map {
      cookie =>
        for {
          uuid    <- Uuid.parse(cookie.cookieValue).toSuccess(
            Client.malformedArgument("session",
                                     "Session cookie did not contain a valid UUID")
          ).fv
          session <- action.read(uuid)
        } yield (Some(session.user) : Option[U])
    } getOrElse (None : Option[U]).success[Problem].fv
  }

}
