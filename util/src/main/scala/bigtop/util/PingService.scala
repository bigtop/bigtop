package bigtop.util

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import blueeyes.concurrent._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service.RestPathPatternParsers.RegexPathPattern
import blueeyes.core.service._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import java.io.File
import scala.util.matching.Regex
import com.weiglewilczek.slf4s.Logging

case class PingService(val path: String = "/api/ping/v1") extends HttpRequestHandlerCombinators
  with BijectionsChunkJson
  with AkkaDefaults
  with Logging
{
  val service: HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]] = {
    path(path) {
      produce(application/json) {
        get { req: HttpRequest[ByteChunk] =>
          logger.error("GET " + req.uri)
          Future(HttpResponse[JValue](content = Some(("ping" -> "pong"))))
        } ~
        post { req: HttpRequest[ByteChunk] =>
          logger.error("POST " + req.uri)
          Future(HttpResponse[JValue](content = Some(("ping" -> "pong"))))
        }
      }
    }
  }
}
