package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data._
import blueeyes.core.service._
import scalaz.{Success, Validation}

case class ExceptionHandlerService[A, B](handler: PartialFunction[Throwable, Future[HttpResponse[B]]])(h: HttpService[A, Future[HttpResponse[B]]]) extends CustomHttpService[A, Future[HttpResponse[B]]] {

  val metadata = None

  val service: HttpRequest[A] => Validation[NotServed, Future[HttpResponse[B]]] =
    (req: HttpRequest[A]) => {
      try {
        h.service(req)
      } catch new PartialFunction[Throwable, Validation[NotServed, Future[HttpResponse[B]]]] {
        def isDefinedAt(exn: Throwable) = handler.isDefinedAt(exn)

        def apply(exn: Throwable) = Success(handler(exn))
      }
    }

}

trait ExceptionHandlerCombinators {
  def handleExceptions[A, B](handler: PartialFunction[Throwable, Future[HttpResponse[B]]])(h: HttpService[A, Future[HttpResponse[B]]]): HttpService[A, Future[HttpResponse[B]]] =
    ExceptionHandlerService(handler)(h)
}
