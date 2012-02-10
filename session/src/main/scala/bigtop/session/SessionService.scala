package bigtop
package session

import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkString, BijectionsIdentity}
import blueeyes.core.service.{ServerHealthMonitorService, HttpRequestHandler, HttpRequestHandler2, HttpServiceContext}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Scalaz, Validation, Success, Failure}
import Scalaz._
import bigtop.json.{Implicits=>JsonImplicits}
import bigtop.concurrent.{FutureValidation, Implicits=>FutureImplicits}

trait SessionService
     extends BlueEyesServiceBuilder
     with HttpRequestCombinators
     with BijectionsChunkJson
     with BijectionsChunkString
{
  val sessionService =
    service("session", "1.0.0") {
      requestLogging {
        healthMonitor { monitor => context =>
          request {
            path("/session/v1") {
              path("/new") {

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
