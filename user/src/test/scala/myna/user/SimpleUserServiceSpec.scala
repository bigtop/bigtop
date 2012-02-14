package bigtop
package user


import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.core.http.HttpResponse
import blueeyes.core.http.MimeTypes._
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._
import bigtop.concurrent.FutureValidation
import scalaz.Validation
import scalaz.syntax.validation._

class SimpleUserServiceSpec extends UserServiceSpecification
    with SimpleUserService
{
  import ProblemWriters._

  def getValue[A](f: Future[A]) = {
    val ans = Await.result(f, Duration("3s"))
    ans
  }

  override def initialize() {
    super.initialize()
  }

  "/user/v1/new" should {

    "return new user given username and password" in initialized {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/user/v1/new")(body)
      val response = getValue(f)

      response must beOk
      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "noel"))
    }

    "return error given bad input" in initialized {
      val body: JValue = ("froobarname" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/user/v1/new")(body)
      val response = getValue(f)

      response must beBadRequest(Request.NoUser)
    }

    "refuse to allow an existing user to be created" in initialized {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")

      val response: FutureValidation[Problem[String], HttpResponse[JValue]] =
        for {
          actions <- userActions.map(_.success[Problem[String]]).fv
          user <- actions.create(body)
          response <- service.contentType[JValue](application/json).post("/user/v1/new")(body).map(_.success[Problem[String]]).fv
        } yield response

       response.inner must whenDelivered {
        (response: Validation[Problem[String], HttpResponse[JValue]]) =>
          response must beSuccess(beBadRequest(Request.UserExists))
      }
    }

  }

  "/user/v1/login" should {

    "return user profile given correct credentials" in initialized {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")

      val response: FutureValidation[Problem[String], HttpResponse[JValue]] =
        for {
          actions <- userActions.map(_.success[Problem[String]]).fv
          user <- actions.create(body)
          response <- service.contentType[JValue](application/json).post("/user/v1/noel/login")(body).map(_.success[Problem[String]]).fv
        } yield response

      response.inner must whenDelivered {
        (response: Validation[Problem[String], HttpResponse[JValue]]) =>
          response must beSuccess()
      }
    }

  }

}
