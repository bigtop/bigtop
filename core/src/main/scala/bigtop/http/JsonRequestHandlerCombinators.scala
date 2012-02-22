package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http.HttpResponse
import blueeyes.core.service.HttpService
import blueeyes.json.JsonAST.JValue

trait JsonRequestHandlerCombinators {

  def json(h: HttpService[Future[JValue], Future[HttpResponse[JValue]]]) =
    JsonService(h)

}
