package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.http._
import bigtop.json.JsonFormatters
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.problem.Problems._
import scalaz.syntax.validation._
import com.weiglewilczek.slf4s.Logging

// Interface

trait SessionServices[U <: User] extends Logging {

  val create: HttpService[Future[JValue],Future[HttpResponse[JValue]]]

  val read: HttpService[Future[JValue],Future[HttpResponse[JValue]]]

  def service =
    JsonSyncService(
      "session",
      "/api/session/v1",
      create,
      read
    )(logger)

}

// Implementation

case class SessionServicesBuilder[U <: User](val actions: SessionActions[U], val auth: Authorizer[U]) extends SessionServices[U] {

  val sessionCreate = SessionCreateService(actions)
  val sessionRead   = SessionReadService(actions, auth)

  val create = sessionCreate.create
  val read   = sessionRead.read

}

trait SessionService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonFormatters


case class SessionCreateService[U <: User](val actions: SessionActions[U]) extends SessionService[U] {

  val create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          json     <- req.json
          username <- json.mandatory[String]("username").fv
          password <- json.mandatory[String]("password").fv
          result   <- actions.create(username, password)
        } yield result).toResponse(actions.externalFormat)
    }

}


case class SessionReadService[U <: User](val actions: SessionActions[U], val auth: Authorizer[U]) extends SessionService[U] {

  /** Only the user that created this session can read */
  val canRead: SecurityCheck[Future[JValue],U] =
    SecurityCheck(
      (req: HttpRequest[Future[JValue]], user: Option[U]) =>
        for {
          id      <- req.mandatoryParam[Uuid]('id).fv
          session <- actions.read(id)
          user    <- if(user.map(_ == session.effectiveUser).getOrElse(false))
                       user.success[Problem].fv
                     else
                       Client.notAuthorized(user.map(_.username).getOrElse("unknown"),
                                            "session.read").fail.fv
        } yield user
    )

  val read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user    <- auth.authorize(req, canRead)
          id      <- req.mandatoryParam[Uuid]('id).fv
          session <- actions.read(id)
        } yield session).toResponse(actions.externalFormat)
    }

}
