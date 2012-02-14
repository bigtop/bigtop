package bigtop
package session

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.json.{JsonImplicits, JsonFormatters, JsonWriter}
import bigtop.problem._
import bigtop.user.User
import bigtop.util.Uuid
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.service.{ServerHealthMonitorService, HttpServiceHandler,  ServiceContext, HttpService}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
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

  def sessionActions: SessionActions[U]

  implicit def defaultTimeout = Timeout(3 seconds)

  /** Get content as JSON and transform to future validation */
  def getContent(request: HttpRequest[Future[JValue]]): FutureValidation[Problem[String], JValue] =
    request.content.fold(
      some = _.map(_.success[Problem[String]]).fv,
      none = (Request.NoContent : Problem[String]).fail[JValue].fv
    )

  val sessionService =
    service("session", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          request {
            path("/api/session/v1") {
              path("/'id") {
                produce(application/json) {
                  (req: HttpRequest[ByteChunk]) =>
                    val result =
                      for {
                        id <- Uuid.parse(req.parameters('id))
                      } yield HttpResponse[JValue](
                        content = Some(
                          ("typename" -> "session") ~
                          ("id"       -> id.toJson) ~
                          ("username" -> "dave") ~
                          ("name"     -> "Joe Bloggs")
                        )
                      )

                    Future {
                      result fold (
                        failure = e => bigtop.problem.BadRequest(e).toResponse,
                        success = x => x
                      )
                    }
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
              jvalue {
                (req: HttpRequest[Future[JValue]]) =>
                  val result =
                    for {
                      json     <- getContent(req)
                      username <- json./[Problem[String],String]("username", Request.NoUser).fv
                    } yield HttpResponse[JValue](
                      content = Some(
                        ("typename" -> "session") ~
                        ("id"       -> Uuid.create.toJson) ~
                        ("username" -> "dave@untyped.com") ~
                        ("name"     -> "Joe Bloggs")
                      )
                    )

                  result fold (
                    failure = e => e.toResponse,
                    success = x => x
                  )
              }
            }
          }
        }
      }
    }
}
