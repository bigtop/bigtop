package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.http.HttpRequest
import blueeyes.core.service.HttpRequestHandlerCombinators
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
import net.lag.logging.Logger

trait UserServices[U <: User] extends UserCreateService[U]
    with UserReadService[U]
    with UserUpdateService[U]
    with UserDeleteService[U]
{

  def action: UserActions[U]

  def service =
    JsonSyncService(
      "user",
      "/api/user/v1",
      create,
      read,
      update,
      delete
    )(Logger.apply)

}


trait UserService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonFormatters
{

  def authorizer: Authorizer[U]

}


trait UserCreateService[U <: User] extends UserService[U] {

  def action: UserCreate[U]

  def canCreate: SecurityCheck[Future[JValue],U]

  def create =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        authorizer.authorize(req, canCreate) {
          user =>
            for {
              json <- req.json
              user <- action.create(json)
            } yield user
        }.toResponse(action.core.serializer)
    }

}


trait UserReadService[U <: User] extends UserService[U] {

  def action: UserRead[U]

  def canRead: SecurityCheck[Future[JValue],U]

  def read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        authorizer.authorize(req, canRead) {
          user =>
            for {
              id   <- req.mandatoryParam[Uuid]('id).fv
              user <- action.read(id)
            } yield user
        }.toResponse(action.core.serializer)
    }

}


trait UserUpdateService[U <: User] extends UserService[U] {

  def action: UserUpdate[U]

  def canUpdate: SecurityCheck[Future[JValue],U]

  def update =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        authorizer.authorize(req, canUpdate) {
          user =>
            for {
              id   <- req.mandatoryParam[Uuid]('id).fv
              data <- req.json
              _    <- action.update(id, data)
            } yield ("status" -> "ok"): JValue
        }.toResponse
    }

}


trait UserDeleteService[U <: User] extends UserService[U] {

  def action: UserDelete[U]

  def canDelete: SecurityCheck[Future[JValue],U]

  def delete =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        authorizer.authorize(req, canDelete) {
          user =>
            for {
              id <- req.mandatoryParam[Uuid]('id).fv
              _  <- action.delete(id)
            } yield ("status" -> "ok"): JValue
        }.toResponse
    }

}
