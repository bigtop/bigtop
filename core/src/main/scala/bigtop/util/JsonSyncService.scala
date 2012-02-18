package bigtop
package util

import akka.dispatch.{Future, Promise}
import bigtop.problem.Problems
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.core.data._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import net.lag.logging.Logger
import scalaz._
import scalaz.syntax.validation._

/**
 * Implement a service that responds to Backbone Sync's URL expectations using JSON content.
 */
object JsonSyncService extends HttpRequestHandlerCombinators
    with BijectionsChunkFutureJson
    with BijectionsChunkJson
    with BijectionsChunkString
    with AkkaDefaults
{

  type Inner = HttpService[Future[JValue],Future[HttpResponse[JValue]]]
  type Outer = HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]]

  def apply(
    name   : String,
    prefix : String,
    create : Inner = dummyHandler("create"),
    read   : Inner = dummyHandler("read"),
    update : Inner = dummyHandler("update"),
    delete : Inner = dummyHandler("delete"),
    search : Inner = dummyHandler("search")
  )(implicit log: Logger) : Outer = SyncService(
    name   = name,
    prefix = prefix,
    create = jvalue(create),
    read   = jsonOut(read),
    update = jvalue(update),
    delete = jsonOut(delete),
    search = jsonOut(search)
  )

  def dummyHandler(action: String): Inner =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        for {
          json <- req.content.getOrElse(Future(JNull))
        } yield {
          HttpResponse(content = Some(
            ("model" -> "channel") ~
            ("action" -> action) ~
            ("request" -> json)
          ))
        }
    }

  def jsonOut(inner: Inner): Outer =
    new CustomHttpService[ByteChunk,Future[HttpResponse[ByteChunk]]] {
      val metadata = None

      val service: HttpRequest[ByteChunk] => Validation[NotServed,Future[HttpResponse[ByteChunk]]] =
        (req: HttpRequest[ByteChunk]) => {
          val innerRes: Validation[NotServed,Future[HttpResponse[JValue]]] =
            try {
              inner.service(req.copy(content = req.content.map(chunk => chunkToFutureJValue()(chunk))))
            } catch {
              case exn: JsonParser.ParseException =>
                Future(Problems.Client.malformedRequest.toResponse).success[NotServed]
            }

          innerRes.map {
            futureResponse =>
              futureResponse.map {
                res =>
                  res.copy(content = res.content.map(json => JValueToChunk.apply(json)))
              }
          }
        }
    }
}
