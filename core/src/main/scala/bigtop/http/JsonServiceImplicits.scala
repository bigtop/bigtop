package bigtop
package http

import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.json.JsonAST._
import scalaz.Validation
import scalaz.syntax.validation._
import bigtop.concurrent._
import bigtop.problem._
import bigtop.json.JsonWriter
import com.weiglewilczek.slf4s.Logger

/** A "wide" HttpRequest with JValue content */
trait JsonHttpRequestW extends HttpRequestW[Future[JValue]] {

  import Problems._
  import FutureImplicits._

  def json: FutureValidation[JValue] =
    request.content match {
      case Some(x) => x.map(_.success[Problem]).fv
      case None    => Problems.Client.emptyRequest.fail.fv
    }

}

trait FutureJsonHttpResponseW[A] {

  val response: FutureValidation[A]

  def toResponse(implicit w: JsonWriter[A], logger: Logger): Future[HttpResponse[JValue]] =
    response.fold (
      fail = _.toResponse,
      succ = v => HttpResponse[JValue](content = Some(w.write(v)))
    )

}

trait FutureJsonHttpResponseSeqW[A] {

  val response: FutureValidation[Seq[A]]

  def toResponseSeq(implicit w: JsonWriter[A], logger: Logger): Future[HttpResponse[JValue]] = {
    response.fold (
      fail = prob => prob.toResponse,
      succ = seq  => HttpResponse[JValue](content = Some(JArray(seq.map(w.write _).toList)))
    )
  }

}

/** Useful functions if you're writing services that take and return JSON */
trait JsonServiceImplicits extends RequestParameterImplicits {

  implicit def httpRequestToHttp(req: HttpRequest[Future[JValue]]): JsonHttpRequestW =
    new JsonHttpRequestW {
      val request = req
    }

  implicit def fvpToHttp[A](resp: FutureValidation[A]): FutureJsonHttpResponseW[A] =
    new FutureJsonHttpResponseW[A] {
      val response = resp
    }

  implicit def fvpSeqToHttp[A](resp: FutureValidation[Seq[A]]): FutureJsonHttpResponseSeqW[A] =
    new FutureJsonHttpResponseSeqW[A] {
      val response = resp
    }

}

object JsonServiceImplicits extends JsonServiceImplicits
