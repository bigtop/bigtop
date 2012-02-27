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

trait SessionCookieAuthorizer[U <: User] extends Authorizer[U]
    with HttpHeaderImplicits
    with FutureImplicits
{

  def action: SessionRead[U]

  def authenticate[A](request: HttpRequest[A]): FutureValidation[Problem,Option[U]] = {
    SessionCookie.get(request) map {
      cookie =>
        for {
          uuid <- Uuid.parse(cookie.cookieValue).toSuccess(
            Client.malformedArgument("session",
                                     "Session cookie did not contain a valid UUID")
          ).fv
          session <- action.read(uuid)
        } yield (Some(session.user) : Option[U])
    } getOrElse (None : Option[U]).success[Problem].fv
  }

}

object SessionCookie {

  val name = "session"

  def get[A](request: HttpRequest[A]): Option[HttpCookie] = {
    val cookies: List[HttpCookie] =
      request.headers.header[Cookie].map(_.cookies).getOrElse(Nil)
    cookies.filter(_.name == name).headOption
  }

}