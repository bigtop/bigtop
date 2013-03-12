package bigtop
package http

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import bigtop.concurrent._
import bigtop.json._
import bigtop.util._
import bigtop.problem._
import blueeyes.Environment
import blueeyes.concurrent.test._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
//import blueeyes.persistence.mongo.ConfigurableMongo
import org.streum.configrity.Configuration
import org.specs2.mutable.Specification
import org.specs2.specification.{Fragment, Fragments, Step}
import scalaz._

trait JsonServiceSpec extends Specification
  with AkkaDefaults
  with SafeBijectionsChunkFutureJson
  with BijectionsChunkJson
  with ResponseMatchers
  with ValidationMatchers
  with FutureImplicits
  with FutureMatchers
  with JsonFormatters
{
  implicit val queryDuration = Duration("3s")
  implicit val queryTimeout = Timeout(queryDuration)

  private val mockSwitch = sys.props.get(Environment.MockSwitch)

  private val specBefore = Step {
    sys.props.getOrElseUpdate (Environment.MockSwitch, "true")
  }

  private val specAfter = Step {
    def setProp(key: String, value: Option[String]) = value match{
      case Some(x) => sys.props.put(key, x)
      case None => sys.props.remove(key)
    }
    setProp(Environment.MockSwitch, mockSwitch)
  }

  override def map(fs: =>Fragments) = specBefore ^ fs ^ specAfter


  def configure(configuration: String) =
    Configuration.parse(configuration)

  def client(service: HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]]) = new DummyClient(service)

  def service: HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]]

  def doGet(url: String, httpClient: HttpClient[ByteChunk] = client(service)): HttpResponse[JValue] =
    httpClient.contentType[JValue](application/json).get[JValue](url).await

  def doPost(url: String, httpClient: HttpClient[ByteChunk] = client(service))(body: JValue): HttpResponse[JValue] =
    httpClient.contentType[JValue](application/json).post(url)(body).await

  def doPut(url: String, httpClient: HttpClient[ByteChunk] = client(service))(body: JValue): HttpResponse[JValue] =
    httpClient.contentType[JValue](application/json).put(url)(body).await

  def doDelete(url: String, httpClient: HttpClient[ByteChunk] = client(service)): HttpResponse[JValue] =
    httpClient.contentType[JValue](application/json).delete[JValue](url).await
}
