package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.service.{AsyncHttpService, HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ByteChunk, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonDSL._
import bigtop.json.{JsonImplicits, JsonWriter, JsonFormatters}
import bigtop.concurrent.{FutureImplicits, FutureValidation}
import bigtop.problem.{Problem, Problems, ProblemWriters}
import bigtop.util.Uuid
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

  def getUser(req: HttpRequest[Future[JValue]]) =
    req.parameters.get('user).toSuccess[Problem](Problems.Client.NoUser).fv

  /** Get content as JSON and transform to future validation */
  def getContent[T](request: HttpRequest[Future[T]]): FutureValidation[Problem, T] =
    request.content.fold(
      some = _.map(_.success[Problem]).fv,
      none = (Problems.Client.NoContent : Problem).fail[T].fv
    )

  def respond[T](f: HttpServiceHandler[Future[JValue], FutureValidation[Problem,T]])
                (implicit w: JsonWriter[T]):
                  HttpService[Future[JValue], Future[HttpResponse[JValue]]] = {
    (req: HttpRequest[Future[JValue]]) => f(req).fold (
      failure = _.toResponse,
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }


  def apply[U <: User](sessionActions: SessionActions[U], userActions: UserActions[U])(implicit w: SessionWriter[U]): AsyncHttpService[ByteChunk] =
    path("/api") {
      path("/session/v1") {
        path("/'id") {
          produce(application/json) {
            (req: HttpRequest[ByteChunk]) =>
              val result =
                for {
                  id <- Uuid.parse(req.parameters('id)).fv.mapFailure(msg => Problems.Client.NoSession)
                } yield HttpResponse[JValue](
                  content = Some(
                    ("typename" -> "session") ~
                    ("id"       -> id.toJson) ~
                    ("username" -> "dave") ~
                    ("name"     -> "Joe Bloggs")
                  )
                )

              result fold (
                failure = e => e.toResponse,
                success = x => x
              )
          }
        } ~
        path("/set") { req: HttpRequest[ByteChunk] =>
          Future(HttpResponse[ByteChunk]())
        } ~
        path("/delete") { req: HttpRequest[ByteChunk] =>
          Future(HttpResponse[ByteChunk]())
        } ~
        path ("/valid") { req: HttpRequest[ByteChunk] =>
          Future(HttpResponse[ByteChunk]())
        } ~
        // Create a session:
        //
        //  username x password -> session
        jvalue {
          (req: HttpRequest[Future[JValue]]) =>
            val session =
              for {
                json     <- getContent(req)
                username <- json./[Problem,String]("username", Problems.Client.NoUser).fv
                password <- json./[Problem,String]("password", Problems.Client.NoPassword).fv
                result   <- sessionActions.create(username, password)
              } yield result

            session fold (
              failure = f => f.toResponse,
              success = s => HttpResponse(content = Some(s.toJson))
            )
        }
      } ~
      path("/user/v1") {
        path("/new") {
          jvalue {
            respond(
              req =>
                for {
                  data <- getContent(req)
                  user <- userActions.create(data)
                } yield userActions.core.serializer.write(user)
            )
          }
        } ~
        path("/(?<user>[^/]*)") {
         // path("/login") {
         //   jvalue {
         //     respond(
         //       req =>
         //         for {
         //           name <- getUser(req)
         //           json <- getContent(req)
         //           pwd  <- json./[Problem,String]("password", Client.NoPassword).fv
         //           user <- userActions.loginUser(name, pwd)
         //         } yield userActions.userFormatter.write(user)
         //     )
         //   }
         // } ~
          path("/update") {
            jvalue {
              respond(
                req =>
                  for {
                    name <- getUser(req)
                    data <- getContent(req)
                      _    <- userActions.update(name, data)
                  } yield JNothing: JValue
              )
            }
          } ~
          path("/delete") {
            jvalue {
              respond(
                req =>
                  for {
                    name <- getUser(req)
                      _    <- userActions.delete(name)
                  } yield JNothing: JValue
              )
            }
          }
        }
      }
    }

}
