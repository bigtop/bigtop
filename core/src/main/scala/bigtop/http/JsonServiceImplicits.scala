package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.json.JsonAST.JValue
import scalaz.Validation
import scalaz.syntax.validation._
import bigtop.concurrent._
import bigtop.problem._

/** A "wide" HttpRequest with JValue content */
trait JsonHttpRequestW extends HttpRequestW[Future[JValue]] {

  import Problems._
  import FutureImplicits._

  def content: FutureValidation[Problem,JValue] =
    request.content.fold(
      some = _.map(_.success[Problem]).fv,
      none = Problems.Client.emptyRequest.fail[T].fv
    )

}

trait FutureJsonHttpResponseW[A] {

  val response: FutureValidation[Problem,A]

  def respond(implicit w: JsonWriter[A]): Future[HttpResponse[JValue]] =
    response.fold (
      failure = _.toResponse
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )

  def respondSeq(implicit w: JsonWriter[A]): Future[HttpResponse[JValue]] = {
    response.fold (
      failure = prob => prob.toResponse,
      success = seq  => HttpResponse[JValue](content = Some(JArray(seq.map(w.write _).toList)))
    )


}

/** Useful functions if you're writing services that take and return JSON */
trait JsonServiceImplicits extends RequestParameterImplicits {

  implicit def httpRequestToHttp(req: HttpRequest[Future[JValue]]): JsonHttpRequestW =
    new JsonHttpRequestW {
      val request = req
    }

  implicit def fvpToHttp[A](resp: FutureValidation[Problem,A]): FutureJsonHttpResponseW[A] =
    new FutureJsonHttpResponseW[A] {
      val response = resp
    }

}

object JsonServiceImplicits extends JsonServiceImplicits
