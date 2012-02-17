package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import bigtop.util._
import bigtop.concurrent._
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._
import blueeyes.core.http._
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.ConfigurableMongo
// import scalaz.syntax.validation._

class SimpleUserServiceSpec extends JsonServiceSpec with SimpleUserService with ConfigurableMongo {

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

  lazy val mongoConfig = rootConfig.configMap("services.user.v1")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database("user")

  def createUser(username: String, password: String): String = {
    val json: JValue =
      doPost("/api/user/v1") {
        ("username" -> username) ~
        ("password" -> password)
      }.content.get

    try {
      (json \ "username" --> classOf[JString]).value
    } catch {
      case _ => sys.error("Could not create test user '%s'. Response: %s".format(username, jsonString(json)))
    }
  }

  def deleteUser(username: String): Unit = {
    val response: HttpResponse[JValue] =
      doDelete("/api/user/v1/%s".format(username))

    if(response.status.code != HttpStatusCodes.OK.code) {
      sys.error("Could not delete test user '%s'".format(username))
    }
  }

  def login(username: String, password: String): Uuid = {
    val json: JValue =
      doPost("/api/session/v1") {
        ("username" -> username) ~
        ("password" -> password)
      }.content.get

    Uuid((json \ "id" --> classOf[JString]).value)
  }

  def initialized[T](f: => T) = {
    database(remove.from("users"))
    deleteUser("dave")
    deleteUser("noel")
    f
  }

  "POST /api/user/v1 (create user)" should {
    "return new user given username and password" in initialized {
      doPost("/api/user/v1") {
        ("username" -> "noel") ~
        ("password" -> "secret")
      } must beOk {
        ("typename" -> "simpleuser") ~
        ("username" -> "noel")
      }
    }

    "return error given bad input" in initialized {
      doPost("/api/user/v1") {
        ("froobarname" -> "noel") ~
        ("password" -> "secret")
      } must beProblem {
        Client.missingArgument("username")
      }
    }

    "refuse to allow an existing user to be created" in initialized {
      createUser("dave", "supersecret")

      doPost("/api/user/v1") {
        ("username" -> "dave") ~
        ("password" -> "supersecret")
      } must beProblem {
        Client.exists("user")
      }
    }
  }

  "GET /api/user/v1/'id (read user)" should {
    "return preexisting user" in initialized {
      createUser("dave", "supersecret")

      doGet("/api/user/v1/dave") must beOk {
        ("typename" -> "simpleuser") ~
        ("username" -> "dave")
      }
    }

    "fail to lookup another user" in initialized {
      createUser("dave", "supersecret")

      doGet("/api/user/v1/noel") must beProblem {
        Client.notFound("user")
      }
    }
  }

  "POST /api/session/v1 (create session, aka login)" should {
    "return existing user given username and password" in initialized {
      createUser("dave", "supersecret")

      doPost("/api/session/v1") {
        ("username" -> "dave") ~
        ("password" -> "supersecret")
      } must beOk(beLike[JValue] {
        case json =>
          (json \ "typename") mustEqual JString("session")
          (json \ "session")  mustEqual JObject(Nil)
          (json \ "user")     mustEqual {
            ("typename" -> "simpleuser") ~
            ("username" -> "dave")
          }
          (json \ "password") mustEqual JNothing
      })
    }

    "return error given incorrect username" in initialized {
      createUser("dave", "supersecret")

      // Log in with their password:
      doPost("/api/session/v1") {
        ("username" -> "noel") ~
        ("password" -> "supersecret")
      } must beProblem {
        Client.loginIncorrect
      }
    }

    "return error given incorrect username" in initialized {
      createUser("dave", "supersecret")

      // Log in with their username:
      doPost("/api/session/v1") {
        ("username" -> "dave") ~
        ("password" -> "superwrong")
      } must beProblem {
        Client.loginIncorrect
      }
    }

    "return error when missing username" in initialized {
      doPost("/api/session/v1") {
        ("password" -> "secret")
      } must beProblem {
        Client.missingArgument("username")
      }
    }

    "return error when missing password" in initialized {
      doPost("/api/session/v1") {
        ("username" -> "dave")
      } must beProblem {
        Client.missingArgument("password")
      }
    }
  }

  "GET /api/session/v1/'id (read session, aka re-authenticate)" should {
    "return a preexisting session" in initialized {
      createUser("dave", "supersecret")

      val sessionId = login("dave", "supersecret")

      doGet("/api/session/v1/%s".format(sessionId)) must beOk {
        ("typename" -> "session") ~
        ("id" -> sessionId.toString) ~
        ("session" -> JObject.empty) ~
        ("user" -> {
          ("typename" -> "simpleuser") ~
          ("username" -> "dave")
        })
      }
    }
  }

}
