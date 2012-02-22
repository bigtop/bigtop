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

trait SessionServices[U <: User] extends SessionCreateService[U]
    with SessionReadService[U] {

  def action: SessionActions[U]

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

trait SessionReadService[U <: User] extends HttpRequestHandlerCombinators
    with JsonServiceImplicits
    with FutureImplicits
    with JsonImplicits
{

  def action: SessionRead[U]

  val read =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        (for {
          id      <- req.mandatoryParam[Uuid]('id).fv
          session <- action.read(id)
        } yield session).toResponse(action.core.externalFormat)
    }

}
