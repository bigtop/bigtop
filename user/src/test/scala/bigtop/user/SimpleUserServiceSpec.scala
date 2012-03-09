package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import bigtop.util._
import bigtop.http._
import bigtop.concurrent._
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._
import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.service.HttpClient
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.ConfigurableMongo
// import scalaz.syntax.validation._

class SimpleUserServiceSpec extends JsonServiceSpec with ConfigurableMongo {

  val configuration = configure("""
    mongo {
      servers = ["localhost"]
      database = "user"
      collection = ["users"]
    }""")

  val (service, auth, userActions, sessionActions) = SimpleUserService.services(configuration)

  lazy val mongoConfig = configuration
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database("user")

  def login(username: String, password: String) =
    sessionActions.create(username, password).await.fold(
      success = _.id,
      failure = p => sys.error(p.toString)
    )

  def auth(client: HttpClient[ByteChunk], sessionId: Uuid) =
    client.header("Cookie", SessionCookie.name+"="+sessionId.toString)

  def authorized(username: String, password: String) =
    auth(client(service), login(username, password))

  def createUser(user: SimpleUser): Uuid = {
    userActions.store.create(user).await.fold(
      success = u => u.id,
      failure = p => sys.error(p.toString)
    )
  }

  def deleteUser(user: SimpleUser): Unit = {
    userActions.store.delete(user.id)
  }

  var noel = SimpleUser(Uuid.create,
                        "noel",
                        Password.fromPassword("password"),
                        "Noel",
                        "Welsh",
                        true)
  val dave = SimpleUser(Uuid.create,
                        "dave",
                        Password.fromPassword("password"),
                        "Dave",
                        "Gurnell",
                        true)
  val test = SimpleUser(Uuid.create,
                        "test",
                        Password.fromPassword("topsecret"),
                        "Test",
                        "McUser",
                        false)


  def initialized[T](f: => T) = {
    createUser(noel)
    deleteUser(dave)
    deleteUser(test)
    // deleteUser("dave")
    // deleteUser("noel")
    f
  }

  "POST /api/user/v1 (create user)" should {
    "return new user given username and password" in initialized {
      doPost("/api/user/v1", authorized("noel", "password")) {
        test.toJson(SimpleUser.internalFormat)
      } must beOk(beLike[JValue] {
        case json =>
          (json \ "id") match {
            case JString(id) =>
              json mustEqual test.copy(id = Uuid(id)).toJson(SimpleUser.externalFormat)
            case _ => failure("Id not found in JSON "+json)
          }
      })
    }

    "return error given bad input" in initialized {
      doPost("/api/user/v1", authorized("noel", "password")) {
        ("froobarname" -> "noel") ~
        ("password" -> "secret")
      } must beProblem {
        Client.missing("username")
      }
    }

    "refuse to allow an existing user to be created" in initialized {
      doPost("/api/user/v1", authorized("noel", "password")) {
        noel.toJson(SimpleUser.internalFormat)
      } must beProblem {
        Client.exists("user")
      }
    }
  }

  "GET /api/user/v1/'id (read user)" should {
    "return preexisting user" in initialized {
      val id = createUser(dave)

      doGet("/api/user/v1/"+id.toString, authorized("dave", "password")) must beOk {
        dave.copy(id = id).toJson(SimpleUser.externalFormat)
      }
    }

    "fail to lookup another user" in initialized {
      doGet("/api/user/v1/"+dave.id.toString, authorized("noel", "password")) must beProblem {
        Client.notFound("user")
      }
    }
  }

  "POST /api/session/v1 (create session, aka login)" should {
    "return existing user given username and password" in initialized {
      createUser(dave)

      doPost("/api/session/v1") {
        ("username" -> "dave") ~
        ("password" -> "password")
      } must beOk(beLike[JValue] {
        case json =>
          (json \ "typename") mustEqual JString("session")
          (json \ "session")  mustEqual JObject(Nil)
          (json \ "user")     mustEqual {
            dave.toJson(SimpleUser.externalFormat)
          }
          (json \ "password") mustEqual JNothing
      })
    }

    "return error given incorrect username" in initialized {
      createUser(dave)

      // Log in with their password:
      doPost("/api/session/v1") {
        ("username" -> "noel") ~
        ("password" -> "supersecret")
      } must beProblem {
        Client.loginUsernameIncorrect
      }
    }

    "return error given incorrect username" in initialized {
      createUser(dave)

      // Log in with their username:
      doPost("/api/session/v1") {
        ("username" -> "dave") ~
        ("password" -> "superwrong")
      } must beProblem {
        Client.loginPasswordIncorrect
      }
    }

    "return error when missing username" in initialized {
      doPost("/api/session/v1") {
        ("password" -> "secret")
      } must beProblem {
        Client.missing("username")
      }
    }

    "return error when missing password" in initialized {
      doPost("/api/session/v1") {
        ("username" -> "dave")
      } must beProblem {
        Client.missing("password")
      }
    }
  }

  "GET /api/session/v1/'id (read session, aka re-authenticate)" should {
    "return a preexisting session" in initialized {
      createUser(dave)

      val sessionId = login("dave", "password")

      doGet("/api/session/v1/%s".format(sessionId), auth(client(service), sessionId)) must beOk {
        ("typename" -> "session") ~
        ("id" -> sessionId.toString) ~
        ("session" -> JObject.empty) ~
        ("user" -> {
          dave.toJson(SimpleUser.externalFormat)
        })
      }
    }
  }

}
