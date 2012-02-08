package bigtop
package user

import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.concurrent.Future
import blueeyes.core.http.{HttpStatus, HttpResponse}
import blueeyes.core.http.HttpStatusCodes._
import org.specs.matcher.Matcher


class SimpleUserServiceSpec extends BlueEyesServiceSpecification 
    with SimpleUserService 
    with ConfigurableMongo
{
  noDetailedDiffs

  import ErrorImplicits._

  override def configuration = """
    services {
      user {
        v1 {
          mongo {
            servers = ["localhost"]
            database = "user"
            collection = ["users"]

            dropBeforeStart {
              myna = ["users"]
            }
          }
        }
      }
    }
  """

  case class BeBadRequest(expected: Errors) extends Matcher[HttpResponse[JValue]] {
    def apply(response: => HttpResponse[JValue]) =
      response match {
        case HttpResponse(status, _, Some(content), _) if status.code == BadRequest =>
          val expectedContent: JValue = errorsWriter.write(expected)
          (content == expectedContent,
           "Response content \"" + content + "\" is \"" + expectedContent + "\"",
           "Response content \"" + content + "\" is not \"" + expectedContent + "\"")

        case _ =>
          (false, 
           "Response \"" + response + "\" is a BadRequest",
           "Response \"" + response + "\" is not a BadRequest or has no content")
      }
  }

  def getValue[A](f: Future[A]) = {
    f.value must eventually(beSomething)
    f.value.get
  }

  "/v1/user/new" should {

    "return new user give username and password" in {
      val json = ("username" -> "noel") ~ ("password" -> "secret")
      val f = service.post[JValue]("/v1/user/new")(json)
      val response = getValue(f)

      response.status mustEqual HttpStatus(OK)
      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "noel"))
    }

    "return error given bad input" in {
      val json = ("froobarname" -> "noel") ~ ("password" -> "secret")
      val f = service.post[JValue]("/v1/user/new")(json)
      val response = getValue(f)

      response must BeBadRequest(ErrorCode.NoUserGiven)
    } 

  }

}
