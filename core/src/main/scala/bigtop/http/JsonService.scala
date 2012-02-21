package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http.CustomHttpService
import blueeyes.json.JsonAST.JValue

case class JsonService extends CustomHttpService[Future[JValue], JValue] {

  def metadata: Some(RequestHeaderMetadata(Right(`Content-Type`(application/json))))

  val service =
    (request: HttpRequest[Future[JValue]]

}
