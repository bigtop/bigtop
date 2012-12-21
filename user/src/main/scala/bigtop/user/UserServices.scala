package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.service.{HttpRequestHandlerCombinators, HttpService}
import blueeyes.json.JsonAST.JValue
import blueeyes.json.JsonDSL._
import bigtop.concurrent.FutureImplicits
import bigtop.http._
import bigtop.http.RequestParameterImplicits._
import bigtop.json._
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.problem.Problems._
import scalaz.syntax.validation._
import com.weiglewilczek.slf4s.{Logger, Logging}


// Interface

trait UserServices[U <: User] extends Logging {
  def actions: UserActions[U]

  def create : HttpService[Future[JValue], Future[HttpResponse[JValue]]]
  def read   : HttpService[Future[JValue], Future[HttpResponse[JValue]]]
  def update : HttpService[Future[JValue], Future[HttpResponse[JValue]]]
  def delete : HttpService[Future[JValue], Future[HttpResponse[JValue]]]

  def service =
    JsonSyncService(
      name   = "user",
      prefix = "/api/user/v1",
      create = create,
      read   = read,
      update = update,
      delete = delete)(logger)
}

// Implementations

case class UserServicesBuilder[U <: User](
  val actions: UserActions[U],
  val canCreate: SecurityCheck[Future[JValue],U],
  val canRead  : SecurityCheck[Future[JValue],U],
  val canUpdate: SecurityCheck[Future[JValue],U],
  val canDelete: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonUpdater[U]
) extends UserServices[U] {
  val userCreate = UserCreateService(actions, canCreate, auth, externalFormat)
  val userRead   = UserReadService(actions, canRead, auth, externalFormat)
  val userUpdate = UserUpdateService(actions, canUpdate, auth, externalFormat)
  val userDelete = UserDeleteService(actions, canDelete, auth)

  lazy val create = userCreate.create
  lazy val read   = userRead.read
  lazy val update = userUpdate.update
  lazy val delete = userDelete.delete
}

trait UserService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonFormatters
    with Logging
{
  implicit val log = logger

  def auth: Authorizer[U]
}

case class UserCreateService[U <: User](
  val actions: UserActions[U],
  val canCreate: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonFormat[U]
) extends UserService[U] {
  val create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          _       <- auth.authorize(req, canCreate).fv
          json    <- req.json.fv
          unsaved <- externalFormat.read(json).toClientProblem.fv
          saved   <- actions.create(unsaved)
        } yield saved).toResponse(externalFormat, log)
    }
}

case class UserReadService[U <: User](
  val actions: UserActions[U],
  val canRead: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonWriter[U]
) extends UserService[U] {
  def read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user <- auth.authorize(req, canRead)
          id   <- req.mandatoryParam[Uuid]('id).toClientProblem.fv
          user <- actions.read(id)
        } yield user).toResponse(externalFormat, log)
    }
}

case class UserUpdateService[U <: User](
  val actions: UserActions[U],
  val canUpdate: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U],
  val externalFormat: JsonUpdater[U]
) extends UserService[U] {
  def update =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user    <- auth.authorize(req, canUpdate).fv
          id      <- req.mandatoryParam[Uuid]('id).toClientProblem.fv
          json    <- req.json.fv
          unsaved <- actions.read(id)
          updated <- externalFormat.update(unsaved, json).toClientProblem.fv
          saved   <- actions.update(updated)
        } yield saved).toResponse(externalFormat, log)
    }
}

case class UserDeleteService[U <: User](
  val actions: UserActions[U],
  val canDelete: SecurityCheck[Future[JValue],U],
  val auth: Authorizer[U]
) extends UserService[U] {
  def delete =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user <- auth.authorize(req, canDelete)
          id   <- req.mandatoryParam[Uuid]('id).toClientProblem.fv
          _    <- actions.delete(id)
        } yield ("status" -> "ok"): JValue).toResponse
    }
}
