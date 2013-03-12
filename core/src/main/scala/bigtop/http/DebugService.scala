package bigtop
package http

import scala.concurrent.Future
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data._
import blueeyes.core.service._
import blueeyes.json.JsonAST.JValue
import blueeyes.json.JsonParser
import scalaz.{Success, Validation}

case class DebugService[A,B](msg: String)(h: HttpService[A, Future[HttpResponse[B]]]) extends CustomHttpService[A, Future[HttpResponse[B]]] {

  val metadata = None

  val service: HttpRequest[A] => Validation[NotServed,Future[HttpResponse[B]]] =
    (req: HttpRequest[A]) => {
      println("============================================================")
      println(msg + " in")
      println(req.toString)
      println(h)
      val response = h.service(req)
      println(msg + " out")
      println("============================================================")
      response
    }

}

trait DebugRequestHandlerCombinators {
  def debug[A, B](msg: String)(h: HttpService[A, Future[HttpResponse[B]]]) = DebugService(msg)(h)
}
