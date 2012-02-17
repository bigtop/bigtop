package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.MimeTypes._
import bigtop.util.ResponseMatchers
import bigtop.concurrent._
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._
import bigtop.json.JsonImplicits
import scalaz.syntax.validation._

class SimpleUserServiceSpec extends UserServiceSpecification
    with SimpleUserService
    with ConfigurableMongo
    with FutureImplicits
    with JsonImplicits
{
  import ProblemWriters._
  import ResponseMatchers._

  def getValue[A](f: Future[A]) = {
    val ans = Await.result(f, Duration("3s"))
    ans
  }

  "/api/user/v1 (create)" should {

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

      response must beProblem(Client.missingArgument("username"))
    }

    "refuse to allow an existing user to be created" in initialized {
      val body: JValue = ("username" -> "dave") ~ ("password" -> "supersecret")
      val f = service.contentType[JValue](application/json).post("/api/user/v1")(body)
      val response = getValue(f)

      response must beProblem(Client.exists("user"))
    }

  }

  "/api/user/v1/id (read)" should {

    "return preexisting user" in initialized {
      val f = service.contentType(application/json).get[JValue]("/api/user/v1/dave")
      val response = getValue(f)
      println(response)
      // Note: this test fails because the user service is expecting a password in the body. GET requests don't have a body, and authentication should be done via a sesison anyway. Need to think more about the semantics of this.
      response must beOk

      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "dave"))
    }

  }

  "/api/session/v1 (login)" should {

    "return existing user given username and password" in  {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1") {
        testUser
      }

      val response = getValue(f)

      response must beOk

      val content = response.content.getOrElse(JNothing)
      content.mandatory[String]("typename") must beSuccess(be_==("session"))
      content \? "session" must beSome(JObject(List()) : JValue)
      content \? "user"    must beSome("typename" -> "user")
      content \? "password" must beNone
    }

    "return error given incorrect username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "foo") ~ ("password" -> "supersecret")
      }
      val response = getValue(f)

      response must beProblem(Client.loginIncorrect)
    }

    "return error given incorrect username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "dave") ~ ("password" -> "superwrong")
      }
      val response = getValue(f)

      response must beProblem(Client.loginIncorrect)
    }

    "return error when missing username" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("password" -> "superwrong")
      }
      val response = getValue(f)

      response must beProblem(Client.missingArgument("username"))
    }

    "return error when missing password" in initialized {
      val f = service.contentType(application/json).post[JValue]("/api/session/v1"){
        ("username" -> "dave")
      }
      val response = getValue(f)

      response must beProblem(Client.missingArgument("password"))
    }

  }

}
