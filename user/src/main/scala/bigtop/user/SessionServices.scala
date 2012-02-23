package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http.HttpRequest
import blueeyes.core.service.HttpRequestHandlerCombinators
import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.http.{JsonServiceImplicits, JsonRequestHandlerCombinators}
import bigtop.json.JsonImplicits
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.problem.Problems._
import scalaz.syntax.validation._

trait SessionServices[U <: User] extends SessionCreateService[U]
    with SessionReadService[U] {

  def action: SessionActions[U]

}


trait SessionService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonImplicits
{

  def authorizer: Authorizer[U]

}


trait SessionCreateService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonImplicits
{

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


trait SessionReadService[U <: User] extends SessionService[U]
{

  def action: SessionRead[U]

  def sameUser(req: HttpRequest[Future[JValue]])(user: Option[U]): FutureValidation[Problem, Option[U]] =
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
        authorizer.authorize(req, sameUser(req)) { user =>
          for {
            id      <- req.mandatoryParam[Uuid]('id).fv
            session <- action.read(id)
          } yield session
        }.toResponse(action.core.externalFormat)
    }

}
