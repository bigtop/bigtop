package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.http.{JsonServiceImplicits, JsonRequestHandlerCombinators}
import bigtop.json.JsonImplicits
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.problem.Problems._
import scalaz.syntax.validation._

trait SessionServices[U <: User] {

  val create: HttpService[Future[JValue],Future[HttpResponse[JValue]]]

  val read: HttpService[Future[JValue],Future[HttpResponse[JValue]]]

}


trait SessionService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonImplicits


trait SessionCreateService[U <: User] extends SessionService[U] {

  def action: SessionCreate[U]

  val create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          json     <- req.json
          username <- json.mandatory[String]("username").fv
          password <- json.mandatory[String]("password").fv
          result   <- action.create(username, password)
        } yield result).toResponse(action.core.externalFormat)
    }

}


trait SessionReadService[U <: User] extends SessionService[U] {

  def action: SessionRead[U]

  def authorizer: Authorizer[U]

  /** Only the user that created this session can read */
  val canRead: SecurityCheck[Future[JValue],U] =
    (req: HttpRequest[Future[JValue]], user: Option[U]) =>
      for {
        id      <- req.mandatoryParam[Uuid]('id).fv
        session <- action.read(id)
        user    <- if(user.map(_ == session.user).getOrElse(false))
                     user.success[Problem].fv
                   else
                     Client.notAuthorized(user.map(_.username).getOrElse("unknown"),
                                          "session.read").fail.fv
      } yield user

  val read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        authorizer.authorize(req, canRead) { user =>
          for {
            id      <- req.mandatoryParam[Uuid]('id).fv
            session <- action.read(id)
          } yield session
        }.toResponse(action.core.externalFormat)
    }

}
