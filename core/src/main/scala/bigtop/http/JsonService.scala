package bigtop
package http

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
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
import com.weiglewilczek.slf4s.Logger

/**
 * The JsonService wraps handlers that accept JSON input and produce JSON output.
 *
 * We don't require the Content-Type be set on the request, because it's inconvenient for a number of reasons.
 */
case class JsonService(h: HttpService[Future[JValue], Future[HttpResponse[JValue]]])(implicit logger: Logger)
     extends CustomHttpService[ByteChunk, Future[HttpResponse[ByteChunk]]]
     with AkkaDefaults
     with SafeBijectionsChunkJson
     with SafeBijectionsChunkFutureJson
     with HttpRequestHandlerCombinators
{
  val metadata = None

  val service: HttpRequest[ByteChunk] => Validation[NotServed,Future[HttpResponse[ByteChunk]]] =
    (req: HttpRequest[ByteChunk]) => {
      try {
        val copy = req.copy(content = req.content.map(chunk => chunkToFutureJValue(chunk)))
        val result = produce(application/json) { h } service { copy }
        result
      } catch {
        case exn: JsonParser.ParseException => {
          val resp = Problems.Client.malformedRequest.toResponse
          Success(Promise.successful(resp.copy(content = resp.content.map(JValueToChunk))))
        }
      }
    }

}
