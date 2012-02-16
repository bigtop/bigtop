package bigtop
package util

import blueeyes.core.service.{HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import net.lag.logging.Logger

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
object SyncService extends HttpRequestHandlerCombinators {

  def apply[T,S](name: String,
                 prefix: String,
                 creator: HttpServiceHandler[T,S],
                 reader : HttpServiceHandler[T,S],
                 updater: HttpServiceHandler[T,S],
                 deleter: HttpServiceHandler[T,S])(implicit log: Logger) : HttpService[T,S] = {

    def logAndProcess(kind: String, in: HttpServiceHandler[T,S]): HttpServiceHandler[T,S] = {
      req => {
        log.info(name + " servicing " + kind)
        in(req)
      }
    }

    path(prefix) {
      path('id) {
        get {
          logAndProcess("read", reader)
          } ~
        put {
          logAndProcess("update", updater)
        } ~
        delete {
          logAndProcess("delete", deleter)
        }
      } ~
      get {
        logAndProcess("read", reader)
      } ~
      post {
        logAndProcess("create", creator)
      }
    }

  }

}
