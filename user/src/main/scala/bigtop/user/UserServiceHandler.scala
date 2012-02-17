package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.service.{AsyncHttpService, HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonDSL._
import bigtop.json.{JsonImplicits, JsonWriter, JsonFormatters}
import bigtop.concurrent.{FutureImplicits, FutureValidation}
import bigtop.problem.{Problem, Problems, ProblemWriters}
import bigtop.util.{SyncService, Uuid}
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
    req.parameters.get('user).toSuccess[Problem](Problems.Client.missingArgument("username")).fv

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

  def apply[U <: User](sessionActions: SessionActions[U], userActions: UserActions[U])(implicit w: SessionWriter[U]): AsyncHttpService[ByteChunk] = {
    implicit val log = Logger.get

    path("/api") {
      SyncService[ByteChunk, ByteChunk] (
        name = "Session",
        prefix = "/session/v1",
        create =
          jvalue[ByteChunk] {
            respond (
              (req: HttpRequest[Future[JValue]]) =>
                for {
                  json     <- getContent(req)
                  username <- json.mandatory[String]("username").fv
                  password <- json.mandatory[String]("password").fv
                  result   <- sessionActions.create(username, password)
                } yield result
            )
          },
        read =
          /*produce(application/json)*/ jvalue {
            respond (
              req =>
                for {
                  id      <- getId(req)
                  session <- sessionActions.read(id)
                } yield session
            )
          },
        update =
          { req: HttpRequest[ByteChunk] =>
            println("In /session/v1/set")
           Future(HttpResponse[ByteChunk]())
         },
        delete =
          { req: HttpRequest[ByteChunk] =>
            println("In /session/v1/delete")
           Future(HttpResponse[ByteChunk]())
         }
      ) ~
      SyncService(
        name = "User",
        prefix = "/user/v1",
        create =
          jvalue {
            respond(
              req =>
                for {
                  data <- getContent(req)
                  user <- userActions.create(data)
                } yield userActions.core.serializer.write(user)
            )
          },
        read =
          jvalue {
            respond(
              req =>
                for {
                  name <- getUser(req)
                  json <- getContent(req)
                  pwd  <- json.mandatory[String]("password").fv
                  user <- userActions.login(name, pwd)
                } yield userActions.core.serializer.write(user)
            )
          },
        update =
          jvalue {
            respond(
              req => {
                for {
                  name <- getUser(req)
                  data <- getContent(req)
                  _    <- userActions.update(name, data)
                } yield JNothing: JValue
              }
            )
          },
        delete =
          jvalue {
            respond(
              req => {
                println("In /user/v1/'id/delete")
                for {
                  name <- getUser(req)
                  _    <- userActions.delete(name)
                } yield JNothing: JValue
              }
            )
          }
      )
    }
  }
}
