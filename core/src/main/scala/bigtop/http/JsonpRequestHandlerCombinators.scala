package bigtop.http

import com.weiglewilczek.slf4s.Logger
import akka.dispatch.{ Future, Promise }
import blueeyes.core.http._
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.core.data._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser.ParseException
import blueeyes.json.Printer
import blueeyes.persistence.mongo.Database
import bigtop.concurrent.FutureValidation
import bigtop.concurrent.FutureImplicits._
import bigtop.http._
import bigtop.json._
import bigtop.problem.Problem
import com.weiglewilczek.slf4s.Logger
import scalaz.Validation
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._

trait JsonpRequestHandlerCombinators extends HttpRequestHandlerCombinators
  with BijectionsChunkString
  with SafeBijectionsChunkJson
  with SafeBijectionsChunkFutureJson {

  def jsonpOut[A,B](delegate: HttpService[Future[JValue], Future[HttpResponse[JValue]]]): HttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] =
    new CustomHttpService[ByteChunk, Future[HttpResponse[ByteChunk]]] {
      val service = (req: HttpRequest[ByteChunk]) => {
        val request = req.copy(content = req.content.map(chunkToFutureJValue))
        request.parameters.get('callback) match {
          case Some(callback) =>
            for(result <- delegate.service(request)) yield {
              result map { response: HttpResponse[JValue] =>
                response.copy(
                  // Always return JSONP as status 200 to prevent the browser ignoring the result:
                  status  = HttpStatus(OK),
                  headers = response.headers + `Content-Type`(application/javascript),
                  content = response.content.map(json => StringToChunk(callback + "(" + Printer.compact(Printer.render(json)) + ");"))
                )
              }
            }

          case None =>
            for(result <- delegate.service(request)) yield {
              result map { response =>
                response.copy(
                  headers = response.headers + `Content-Type`(application/json),
                  content = response.content.map(JValueToChunk(_))
                )
              }
            }
        }
      }

      val metadata = None
    }
}
