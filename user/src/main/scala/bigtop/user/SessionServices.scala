package bigtop
package user

import akka.dispatch.Future
import bigtop.concurrent.FutureImplicits
import bigtop.http._
import bigtop.http.RequestParameterImplicits.{ tuple => reqTuple, _ }
import bigtop.json._
import bigtop.json.JsonFormatters._
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem._
import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.json.JsonAST._
import scalaz.syntax.validation._
import com.weiglewilczek.slf4s.{Logging, Logger}

// Interface

trait SessionServices[U <: User] extends Logging
  with HttpRequestHandlerCombinators
  with JsonRequestHandlerCombinators
{
  implicit val log = logger

  val create: HttpService[Future[JValue],Future[HttpResponse[JValue]]]
  val read: HttpService[Future[JValue],Future[HttpResponse[JValue]]]
  val changeIdentity: HttpService[Future[JValue],Future[HttpResponse[JValue]]]
  val restoreIdentity: HttpService[Future[JValue],Future[HttpResponse[JValue]]]

  def service =
    path("/api/session/v1/change-identity") {
      json { changeIdentity }
    } ~
    path("/api/session/v1/restore-identity") {
      json { restoreIdentity }
    } ~
    JsonSyncService(
      "session",
      "/api/session/v1",
      'id,
      create,
      read
    )(logger)
}

// Implementation

case class SessionServicesBuilder[U <: User](
  val actions: SessionActions[U],
  val userActions: UserActions[U],
  val canChange: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonWriter[Session[U]]
) extends SessionServices[U] {
  val sessionCreate          = SessionCreateService(actions, externalFormat)
  val sessionRead            = SessionReadService(actions, auth, externalFormat)
  val sessionChangeIdentity  = SessionChangeIdentityService(actions, userActions, canChange, auth, externalFormat)
  val sessionRestoreIdentity = SessionRestoreIdentityService(actions, auth, externalFormat)

  lazy val create          = sessionCreate.create
  lazy val read            = sessionRead.read
  lazy val changeIdentity  = sessionChangeIdentity.changeIdentity
  lazy val restoreIdentity = sessionRestoreIdentity.restoreIdentity
}

trait SessionService[U <: User] extends HttpRequestHandlerCombinators
  with JsonServiceImplicits
  with FutureImplicits
  with Logging
{
  implicit val log = logger
}

case class SessionCreateService[U <: User](
  val actions: SessionActions[U],
  val externalFormat: JsonWriter[Session[U]]
) extends SessionService[U] {
  val create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          json                 <- req.json
          (username, password) <- tuple(
                                    json.mandatory[String]("username"),
                                    json.mandatory[String]("password")
                                  ).toClientProblem.fv
          result               <- actions.create(username, password)
        } yield result).toResponse(externalFormat, log)
    }
}

case class SessionReadService[U <: User](
  val actions: SessionActions[U],
  val auth: Authorizer[U],
  val externalFormat: JsonWriter[Session[U]]
) extends SessionService[U] {
  /** Only the user that created this session can read */
  val canRead: SecurityCheck[Future[JValue],U] =
    SecurityCheck(
      (req: HttpRequest[Future[JValue]], user: Option[U]) =>
        for {
          id      <- req.mandatoryParam[Uuid]('id).toClientProblem.fv
          session <- actions.read(id)
          user    <- if(user.map(_ == session.effectiveUser).getOrElse(false)) {
                       user.success[Problem].fv
                     } else Problems.Authorization(
                       user.map(_.username).getOrElse("unknown"),
                       "session.read"
                     ).fail.fv
        } yield user
    )

  val read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user    <- auth.authorize(req, canRead)
          id      <- req.mandatoryParam[Uuid]('id).toClientProblem.fv
          session <- actions.read(id)
        } yield session).toResponse(externalFormat, log)
    }
}

case class SessionChangeIdentityService[U <: User](
  val actions: SessionActions[U],
  val userActions: UserActions[U],
  val canChange: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonWriter[Session[U]]
) extends SessionService[U]
  with JsonRequestHandlerCombinators
{
  val changeIdentity =
    post {
      service {
        (req: HttpRequest[Future[JValue]]) =>
          (for {
            session            <- auth.mandatorySession(req, "session.changeIdentity")
            user               <- canChange(req, Some(session.realUser))
            json               <- req.json : FutureValidation[JValue]
            (userId, username) <- tuple(
                                    json.optional[Uuid]("id"),
                                    json.optional[String]("username")
                                  ).toClientProblem.fv
            user               <- (userId, username) match {
                                    case (Some(id),   _) => userActions.read(id)
                                    case (_, Some(name)) => userActions.readByUsername(name)
                                    case (_, _)          => JsonErrors(
                                                              JsonError.Missing("id"),
                                                              JsonError.Missing("username")
                                                            ).fail[U].toClientProblem.fv
                                  }
            session            <- actions.changeIdentity(session.id, user)
          } yield session).toResponse(externalFormat, log)
      }
    }
}

case class SessionRestoreIdentityService[U <: User](
  val actions: SessionActions[U],
  val auth: Authorizer[U],
  val externalFormat: JsonWriter[Session[U]]
) extends SessionService[U] {
  val restoreIdentity =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          session <- auth.mandatorySession(req, "session.restoreIdentity")
          session <- actions.restoreIdentity(session.id)
        } yield session).toResponse(externalFormat, log)
    }
}
