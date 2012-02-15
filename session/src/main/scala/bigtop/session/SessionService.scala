package bigtop
package session


import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.json.{JsonImplicits, JsonFormatters, JsonWriter}
import bigtop.problem._
import bigtop.user.{User, UserActions}
import bigtop.util.Uuid
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.service.{AsyncHttpService, ServerHealthMonitorService, HttpServiceHandler,  ServiceContext, HttpService}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Scalaz, Validation, Success, Failure}
import Scalaz._


trait SessionService[U <: User]
     extends BlueEyesServiceBuilder
     with HttpRequestCombinators
     with BijectionsChunkJson
     with BijectionsChunkFutureJson
     with FutureImplicits
     with JsonImplicits
     with JsonFormatters
     with ProblemWriters {

  import Problems._

  implicit def sessionWriter = new SessionWriter[U] {}

  implicit def defaultTimeout: Timeout

  /** Get content and transform to future validation */
  private def getContent[T](request: HttpRequest[Future[T]]): FutureValidation[Problem, T] =
    request.content.fold(
      some = _.map(_.success[Problem]).fv,
      none = (Client.NoContent : Problem).fail[T].fv
    )

  def createUserActions(config: ConfigMap): UserActions[U]

  val sessionService =
    service("session", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          startup {
            Promise.successful(new SessionActions(context.config, createUserActions(context.config)))
          } ->
          request { sessionActions: SessionActions[U] =>
            sessionServiceRequestHandler(sessionActions)
          } ->
          shutdown { config =>
            Promise.successful(())
          }
        }
      }
    }

  def sessionServiceRequestHandler(sessionActions: SessionActions[U]): AsyncHttpService[ByteChunk] =
    path("/api/session/v1") {
      path("/'id") {
        produce(application/json) {
          (req: HttpRequest[ByteChunk]) =>
            val result =
              for {
                id <- Uuid.parse(req.parameters('id)).fv.mapFailure(msg => ClientProblem + ("id" -> msg))
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
    }

}
