package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.service.{AsyncHttpService, HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.http.JsonSyncService
import bigtop.json.{JsonImplicits, JsonWriter, JsonFormatters}
import bigtop.concurrent.{FutureImplicits, FutureValidation}
import bigtop.problem.{Problem, Problems, ProblemWriters}
import bigtop.util.Uuid
import net.lag.logging.Logger
import scalaz.{Validation, Success, Failure}
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import scalaz.std.option._


object UserServiceHandler extends BijectionsChunkJson
    with BijectionsChunkFutureJson
    with FutureImplicits
    with JsonImplicits
    with HttpRequestHandlerCombinators
    with ProblemWriters
    with JsonFormatters
{
  def getId(req: HttpRequest[_]) =
    for {
      id   <- req.parameters.get('id).toSuccess[Problem](Problems.Client.missingArgument("id")).fv
      uuid <- Uuid.parse(id).toSuccess(Problems.Client.malformedArgument(id, "expected uuid, found " + id)).fv
    } yield uuid

  def getUser(req: HttpRequest[Future[JValue]]) =
    req.parameters.get('id).toSuccess[Problem](Problems.Client.missingArgument("id")).fv

  /** Get content as JSON and transform to future validation */
  def getContent[T](request: HttpRequest[Future[T]]): FutureValidation[Problem, T] =
    request.content.fold(
      some = _.map(_.success[Problem]).fv,
      none = Problems.Client.emptyRequest.fail[T].fv
    )

  def respond[B](f: HttpServiceHandler[Future[JValue], FutureValidation[Problem,B]])
                (implicit w: JsonWriter[B]):
                  HttpService[Future[JValue], Future[HttpResponse[JValue]]] = {
    (req: HttpRequest[Future[JValue]]) => f(req).fold (
      failure = _.toResponse,
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }

  def apply[U <: User](sessionServices: SessionServices[U], userActions: UserActions[U]): AsyncHttpService[ByteChunk] = {
    implicit val log = Logger.get

    JsonSyncService(
      name   = "Session",
      prefix = "/api/session/v1",
      create = sessionServices.create,
      read   = sessionServices.read
    ) ~
    JsonSyncService(
      name = "User",
      prefix = "/api/user/v1",
      create =
        respond {
          req =>
            for {
              data <- getContent(req)
              user <- userActions.create(data)
            } yield userActions.core.serializer.write(user)
        },
      read =
        respond {
          req =>
            for {
              name <- getUser(req)
              user <- userActions.read(name)
            } yield userActions.core.serializer.write(user)
        },
      update =
        respond {
          req =>
            for {
              name <- getUser(req)
              data <- getContent(req)
              _    <- userActions.update(name, data)
            } yield ("status" -> "ok"): JValue
        },
      delete =
        respond {
          req =>
            for {
              name <- getUser(req)
              _    <- userActions.delete(name)
            } yield ("status" -> "ok"): JValue
        }
    )
  }
}
