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

class SimpleUserServiceSpec extends BlueEyesServiceSpecification
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

  lazy val mongoConfig = rootConfig.configMap("services.user.v1.mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database("user")

  def initialise() {
    database(remove.from("users"))
  }

  def initialized[T](f: => T) = {
    initialise
    f
  }

  def getValue[A](f: Future[A]) = {
    val ans = Await.result(f, Duration("3s"))
    ans
  }

  "/user/v1/new" should {

    "return new user given username and password" in initialized {
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/user/v1/new")(body)
      val response = getValue(f)

      response must beOk
      response.content must beSome(("typename" -> "simpleuser") ~ ("username" -> "noel"))
    }

    "return error given bad input" in {
      //initialise
      val body: JValue = ("froobarname" -> "noel") ~ ("password" -> "secret")
      val f = service.contentType[JValue](application/json).post("/user/v1/new")(body)
      val response = getValue(f)

      response must beBadRequest(Client.NoUser)
    }

    "refuse to allow an existing user to be created" in {
      //initialise
      val body: JValue = ("username" -> "noel") ~ ("password" -> "secret")

      // Create the user
      userActions map {
        actions =>
          actions.create(body)
      }

      val f = service.contentType[JValue](application/json).post("/user/v1/new")(body)
      val response = getValue(f)

      response must beBadRequest(Client.UserExists)
    }

  }

}
