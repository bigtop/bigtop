package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http.HttpResponse
import blueeyes.core.service.HttpService
import blueeyes.json.JsonAST.JValue
import com.weiglewilczek.slf4s.Logger

trait JsonRequestHandlerCombinators {

  def json(h: HttpService[Future[JValue], Future[HttpResponse[JValue]]])(implicit logger: Logger) =
    JsonService(h)

}
