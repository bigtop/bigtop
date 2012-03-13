package bigtop
package http

import akka.dispatch.{Future, Promise}
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service.{CustomHttpService, HttpService, HttpRequestHandlerCombinators}
import blueeyes.core.data.{Bijection, ByteChunk}
import com.weiglewilczek.slf4s.Logger


case class LoggingService[A,B](name: String, kind: String, log: Logger, h: HttpService[A,B])
     extends CustomHttpService[A,B]
{
  val metadata = None

  val service = (req: HttpRequest[A]) => {
    log.info(name + " servicing " + kind)
    h.service(req)
  }
}

/**
 * Implement a service that responds to Backbone Sync's URL expectations:
 *
 *  create → POST   /collection
 *  read → GET   /collection[/id]
 *  update → PUT   /collection/id
 *  delete → DELETE   /collection/id
 *
 * name is used for logging
 * prefix is the prefix to put in front of the urls, including "collection" above
 * create / read / update / delete are the service handlers
 *
 * The id above is parsed as the parameter "id". I.e. look for request.parameters.get('id)
 */
object SyncService extends HttpRequestHandlerCombinators with AkkaDefaults {

  type FR[B] = Future[HttpResponse[B]]
  type HS[A,B] = HttpService[A, FR[B]]

  def apply[A,B](
    name: String,
    prefix: String,
    create : HS[A,B],
    read   : HS[A,B],
    update : HS[A,B],
    delete : HS[A,B]
  )(implicit log: Logger): HS[A,B] = apply(
    name   = name,
    prefix = prefix,
    create = create,
    read   = read,
    update = update,
    delete = delete,
    search = read
  )(log)

  def apply[A,B](
    name: String,
    prefix: String,
    create : HS[A,B],
    read   : HS[A,B],
    update : HS[A,B],
    delete : HS[A,B],
    search : HS[A,B]
  )(implicit log: Logger) : HS[A,B] = {

    def logAndProcess(kind: String, in: HS[A,B]): HS[A,B] =
      LoggingService(name, kind, log, in)

    path(prefix) {
      path("/'id") {
        get {
          logAndProcess("read", read)
        } ~
        put {
          logAndProcess("update", update)
        } ~
        this.delete {
          logAndProcess("delete", delete)
        }
      } ~
      get {
        logAndProcess("search", search)
      } ~
      post {
        logAndProcess("create", create)
      } ~
      logAndProcess(
        "not found",
        service {
          (req: HttpRequest[A]) =>
            log.info(name + " received request which didn't match any handler " + req)
            Promise successful HttpResponse[B](status = HttpStatus(NotFound))
        }
      )
    }

  }

}
