package bigtop
package util

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import akka.util.Timeout
import bigtop.concurrent._
import bigtop.json._
import bigtop.util._
import bigtop.problem._
import blueeyes.concurrent.test._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.ConfigurableMongo
import scalaz._

trait JsonServiceSpec extends BlueEyesServiceSpecification
  with BijectionsChunkFutureJson
  with BijectionsChunkJson
  with ResponseMatchers
  with ValidationMatchers
  with FutureImplicits
  with FutureMatchers
  with JsonImplicits {

  case class FutureW[A](val f: Future[A]) {
    def await: A =
      Await.result(f, Duration("3s"))
  }

  implicit def futureToFutureW[A](fv: Future[A]) =
    FutureW(fv)

  case class FutureValidationW[E,S](val fv: FutureValidation[E,S]) {
    def await: Validation[E,S] =
      Await.result(fv.inner, Duration("3s"))

    def awaitSuccess: S =
      await.toOption.get
  }

  implicit def futureValidationToFutureValidationW[E,S](fv: FutureValidation[E,S]) =
    FutureValidationW(fv)

  // def convertResponse(in: HttpResponse[ByteChunk])(implicit timeout: Timeout): HttpResponse[JValue] =
  //   in.copy(content = in.content.map(chunk => chunkToFutureJValue(timeout)(chunk).await))

  def doGet(url: String): HttpResponse[JValue] =
    service.contentType[JValue](application/json).get[JValue](url).await

  def doPost(url: String)(body: JValue): HttpResponse[JValue] =
    service.contentType[JValue](application/json).post(url)(body).await

  def doPut(url: String)(body: JValue): HttpResponse[JValue] =
    service.contentType[JValue](application/json).put(url)(body).await

  def doDelete(url: String): HttpResponse[JValue] =
    service.contentType[JValue](application/json).delete[JValue](url).await
}
