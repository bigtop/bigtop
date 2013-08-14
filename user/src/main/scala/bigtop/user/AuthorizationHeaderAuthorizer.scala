package bigtop
package user

import blueeyes.core.http._
import blueeyes.core.http.HttpHeaders._
import bigtop.util.Uuid
import bigtop.concurrent._
import bigtop.problem._
import scalaz.Validation
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

case class AuthorizationHeaderAuthorizer[U <: User](val actions: SessionActions[U]) extends Authorizer[U]
  with HttpHeaderImplicits
  with FutureImplicits
{
  private val BearerRegex = "Bearer (.*)".r

  private def badHeader(credentials: String = "<blank>") = {
    Problems.Authentication(
      credentials = credentials,
      message     = "The authorization header did not contain a valid bearer."
    )
  }

  def effectiveUser[A](request: HttpRequest[A]): FutureValidation[Option[U]] = {
    session(request).map(_.map(_.effectiveUser))
  }

  def realUser[A](request: HttpRequest[A]): FutureValidation[Option[U]] = {
    session(request).map(_.map(_.realUser))
  }

  def session[A](request: HttpRequest[A]): FutureValidation[Option[Session[U]]] = {
    val authHeader: String =
      request.headers.header(HttpHeaders.Authorization).map(_.value).getOrElse("")

    authHeader match {
      case BearerRegex(bearer) =>
        for {
          uuid    <- Uuid.parse(bearer).toSuccess(badHeader(bearer)).fv
          session <- actions.read(uuid)
        } yield (Some(session) : Option[Session[U]])
      case _ =>
        badHeader().fail[Option[Session[U]]].fv
    }
  }
}
