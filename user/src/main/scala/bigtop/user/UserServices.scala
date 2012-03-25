package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.service.{HttpRequestHandlerCombinators, HttpService}
import blueeyes.json.JsonAST.JValue
import blueeyes.json.JsonDSL._
import bigtop.concurrent.FutureImplicits
import bigtop.http._
import bigtop.json._
import bigtop.util.Uuid
import bigtop.concurrent.FutureValidation
import bigtop.problem.Problem
import bigtop.problem.Problems._
import scalaz.syntax.validation._
import com.weiglewilczek.slf4s.Logging


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
  val auth: Authorizer[U]
) extends UserServices[U] {

  val userCreate = UserCreateService(actions, canCreate, auth)
  val userRead   = UserReadService(actions, canRead, auth)
  val userUpdate = UserUpdateService(actions, canUpdate, auth)
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
{

  def auth: Authorizer[U]

}


case class UserCreateService[U <: User](val actions: UserActions[U], val canCreate: SecurityCheck[Future[JValue],U], val auth: Authorizer[U]) extends UserService[U] {

  val create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          _    <- auth.authorize(req, canCreate)
          json <- req.json
          user <- actions.create(json)
        } yield user).toResponse(actions.externalFormat)
    }

}


case class UserReadService[U <: User](val actions: UserActions[U], val canRead: SecurityCheck[Future[JValue],U], val auth: Authorizer[U]) extends UserService[U] {

  def read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user <- auth.authorize(req, canRead)
          id   <- req.mandatoryParam[Uuid]('id).fv
          user <- actions.read(id)
        } yield user).toResponse(actions.externalFormat)
    }

}


case class UserUpdateService[U <: User](val actions: UserActions[U], val canUpdate: SecurityCheck[Future[JValue],U], val auth: Authorizer[U]) extends UserService[U] {

  def update =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user <- auth.authorize(req, canUpdate)
          id   <- req.mandatoryParam[Uuid]('id).fv
          data <- req.json
          _    <- actions.update(id, data)
        } yield ("status" -> "ok"): JValue).toResponse
    }

}


case class UserDeleteService[U <: User](val actions: UserActions[U], val canDelete: SecurityCheck[Future[JValue],U], val auth: Authorizer[U]) extends UserService[U] {

  def delete =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          user <- auth.authorize(req, canDelete)
          id <- req.mandatoryParam[Uuid]('id).fv
          _  <- actions.delete(id)
        } yield ("status" -> "ok"): JValue).toResponse
    }

}
