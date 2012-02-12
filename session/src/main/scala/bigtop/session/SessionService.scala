package bigtop
package session

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.json.{JsonImplicits, JsonFormatters}
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
     with JsonImplicits {

  def sessionActions: SessionActions[U]

  implicit def defaultTimeout = Timeout(3 seconds)

  val sessionService =
    service("session", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          request {
            path("/session/v1") {
              path("/new") {
                // TODO: Fix type errors in this dummy request handler...
                // then replace it with something that actually works!
                //
                // contentType(application/json) {
                //   (req: HttpRequest[Future[ByteChunk]]) =>
                //     Future(
                //       HttpResponse[JValue](
                //         content = Some(
                //           ("sessionkey" -> Uuid.parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa").toOption.get)
                //         )
                //       )
                //     )
                // }
              } ~
              path("/get") {

              } ~
              path("/set") {

              } ~
              path("/delete") {

              } ~
              path ("/valid") {

              }
            }
          }
        }
      }
    }
}
