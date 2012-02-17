package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.MimeTypes._
import bigtop.util.ResponseMatchers
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._

class SimpleUserServiceSpec extends UserServiceSpecification
    with SimpleUserService
    with ConfigurableMongo
{
  import ProblemWriters._
  import ResponseMatchers._

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


  def getValue[A](f: Future[A]) = {
    val ans = Await.result(f, Duration("3s"))
    ans
  }

  "/api/user/v1/" should {

    "return new user given username and password" in initialized {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/api/user/v1")(body)
      val response = getValue(f)

      response must beOk
      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "noel"))
    }

    "return error given bad input" in {
      //initialise
      val body: JValue = ("froobarname" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/api/user/v1")(body)
      val response = getValue(f)

      response must beBadRequest(Client.missingArgument("username"))
    }

    "refuse to allow an existing user to be created" in initialized {
      //initialise
      val body: JValue = ("username" -> "dave") ~ ("password" -> "supersecret")
      val f = service.contentType[JValue](application/json).post("/api/user/v1")(body)
      val response = getValue(f)

      response must beBadRequest(Client.exists("user"))
    }

  }

  "/api/session/v1/ (login)" should {

    "return existing user given username and password" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1") {
        testUser
      }

      val response = getValue(f)

      response must beOk

/*      val json = response.content.get
      json.mandatory[String]("typename", Client.missingArgument("typename")) must beSuccess("session")
      json.mandatory[JValue]("session", Client.noSession) must beSuccess(JObject(List()))
      json.mandatory[JValue]("user", Client.missingArgument("user")) must beSuccess(("typename" -> "user"))*/
    }

    "return error given incorrect username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "foo") ~ ("password" -> "supersecret")
      }
      val response = getValue(f)

      response must beBadRequest(Client.loginIncorrect)
    }

    "return error given incorrect username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "dave") ~ ("password" -> "superwrong")
      }
      val response = getValue(f)

      response must beBadRequest(Client.loginIncorrect)
    }

    "return error when missing username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("password" -> "superwrong")
      }
      val response = getValue(f)

      response must beBadRequest(Client.missingArgument("username"))
    }

    "return error when missing password" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "dave")
      }
      val response = getValue(f)

      response must beBadRequest(Client.missingArgument("password"))
    }

  }

}
