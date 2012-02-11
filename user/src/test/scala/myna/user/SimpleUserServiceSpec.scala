package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.core.http.HttpStatusCodes._
import org.specs2.matcher.Matcher
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._

class SimpleUserServiceSpec extends BlueEyesServiceSpecification
    with SimpleUserService
    with ConfigurableMongo
{
  import MimeTypes._
  import ProblemWriters._

  override def configuration = """
    services {
      user {
        v1 {
          mongo {
            servers = ["localhost"]
            database = "user"
            collection = ["users"]
         }
        }
      }
    }
  """

  def beBadRequest(expected: Problem[String]): Matcher[HttpResponse[JValue]] =
    beEqualTo(expected.toResponse[JValue])

  def getValue[A](f: Future[A]) = {
    val ans = Await.result(f, Duration("3s"))
    ans
  }

  "/v1/user/new" should {

    "return new user give username and password" in {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/v1/user/new")(body)
      val response = getValue(f)

      response.status mustEqual HttpStatus(OK)
      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "noel"))
    }

    "return error given bad input" in {
      val body: JValue = ("froobarname" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/v1/user/new")(body)
      val response = getValue(f)

      response must beBadRequest(Request.NoUser)
    }

  }

}
