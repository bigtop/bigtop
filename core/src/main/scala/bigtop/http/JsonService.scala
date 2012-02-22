package bigtop
package http

import akka.dispatch.{Future, Promise}
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data._
import blueeyes.core.service._
import blueeyes.json.JsonAST.JValue
import blueeyes.json.JsonParser
import scalaz.{Success, Validation}
import scalaz.syntax.validation._
import bigtop.problem.Problems

/**
 * The JsonService wraps handlers that accept JSON input and produce JSON output.
 *
 * We don't require the Content-Type be set on the request, because it's inconvenient for a number of reasons.
 */
case class JsonService(h: HttpService[Future[JValue], Future[HttpResponse[JValue]]])
     extends CustomHttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]
     with AkkaDefaults
     with BijectionsChunkJson
     with BijectionsChunkFutureJson
     with HttpRequestHandlerCombinators
{

  val metadata = None

  val service: HttpRequest[ByteChunk] => Validation[NotServed,Future[HttpResponse[ByteChunk]]] =
    (req: HttpRequest[ByteChunk]) => {
      try {
        produce(application/json) { h } service {
          req.copy(content = req.content.map(chunk => chunkToFutureJValue()(chunk)))
        }
      } catch {
        case exn: JsonParser.ParseException => {
          val resp = Problems.Client.malformedRequest.toResponse
          Success(Promise.successful(resp.copy(content = resp.content.map(JValueToChunk))))
        }
      }
    }

}
