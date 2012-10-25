package bigtop
package http

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
import com.weiglewilczek.slf4s.Logger
import scalaz._
import scalaz.syntax.validation._

/**
 * Implement a service that responds to Backbone Sync's URL expectations using JSON content.
 */
object JsonSyncService extends JsonRequestHandlerCombinators
    with HttpRequestHandlerCombinators
    with SafeBijectionsChunkFutureJson
    with BijectionsChunkJson
    with BijectionsChunkString
    with AkkaDefaults
{

  type Inner = HttpService[Future[JValue],Future[HttpResponse[JValue]]]
  type Outer = HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]]

  def apply(
    name   : String,
    prefix : String,
    create : Inner = dummyHandler,
    read   : Inner = dummyHandler,
    update : Inner = dummyHandler,
    delete : Inner = dummyHandler,
    search : Inner = dummyHandler
  )(implicit log: Logger) : Outer = SyncService(
    name   = name,
    prefix = prefix,
    create = json(create),
    read   = json(read),
    update = json(update),
    delete = json(delete),
    search = json(search)
  )

  val dummyHandler: Inner =
    service {
      (req: HttpRequest[Future[JValue]]) =>
        for {
          json <- req.content.getOrElse(Future(JNull))
        } yield {
          HttpResponse(
            status = HttpStatusCodes.NotFound,
            content = Some(
              ("method"  -> req.method.toString) ~
              ("uri"     -> req.uri.toString) ~
              ("request" -> json)
            )
          )
        }
    }

}
