package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import bigtop.util._
import bigtop.http._
import bigtop.concurrent._
import bigtop.problem.{Problem, ProblemFormat}
import bigtop.problem.Problems._
import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.service.HttpClient
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.ConfigurableMongo
// import scalaz.syntax.validation._

class SimpleUserServiceSpec extends JsonServiceSpec with ConfigurableMongo {
  args.execute(sequential = true)

  val configuration = configure(
    """
    mongo {
      servers = ["localhost"]
      database = "bigtoptest"
      collection = ["users"]
    }
    """
  )

  val (service, auth, userActions, sessionActions) = SimpleUserService.services(configuration)

  def login(username: String, password: String) =
    sessionActions.create(username, password).await.fold(
      success = u => u.id,
      failure = p => sys.error("SimpleUserServiceSpec.login failed: " + p.toString)
    )

  def auth(client: HttpClient[ByteChunk], sessionId: Uuid) =
    client.header("Cookie", SessionCookie.name+"="+sessionId.toString)

  def authorized(username: String, password: String) =
    auth(client(service), login(username, password))

  def createUser(user: SimpleUser): Uuid = {
    userActions.create(user).await.fold(
      success = u => u.id,
      failure = p => sys.error("SimpleUserServiceSpec.createUser failed: " + p.toString)
    )
  }

  def userExists(id: Uuid): Boolean = {
    userActions.read(id).await.fold(
      success = u => true,
      failure = p => false
    )
  }

  def deleteUser(user: SimpleUser): Unit = {
    userActions.delete(user.id)
  }

  def deleteAllUsers = {
    userActions.database(remove.from("users"))
  }

  def allUsers = {
    userActions.database(select().from("users")).await.map(_ \ "username").toList
  }

  var noel = SimpleUser(
    Uuid.create,
    "noel",
    Password.fromPassword("password"),
    "Noel",
    "Welsh",
    true
  )

  val dave = SimpleUser(
    Uuid.create,
    "dave",
    Password.fromPassword("password"),
    "Dave",
    "Gurnell",
    true
  )

  val test = SimpleUser(
    Uuid.create,
    "test",
    Password.fromPassword("topsecret"),
    "Test",
    "McUser",
    false
  )

  def initialized[T](f: => T) = {
    deleteAllUsers.await
    if(userExists(dave.id)) deleteUser(dave)
    if(!userExists(noel.id)) createUser(noel)
    if(!userExists(test.id)) createUser(test)
    f
  }

  "POST /api/user/v1 (create user)" should {
    "return new user given username and password" in initialized {
      doPost("/api/user/v1", authorized("noel", "password")) {
        dave.toJson(SimpleUser.internalFormat)
      } must beOk(beLike[JValue] {
        case json =>
          (json \ "id") match {
            case JString(id) =>
              json mustEqual dave.copy(id = Uuid(id)).toJson(SimpleUser.externalFormat)
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
      doGet("/api/user/v1/"+dave.id.toString, authorized("test", "topsecret")) must beProblem {
        Client.notAuthorized("test", "user.read")
      }
    }
  }

  "POST /api/session/v1 (create session, aka login)" should {
    "return existing user given username and password" in initialized {

      doPost("/api/session/v1") {
        ("username" -> "test") ~
        ("password" -> "topsecret")
      } must beOk(beLike[JValue] {
        case json =>
          (json \ "typename") mustEqual JString("session")
          (json \ "session")  mustEqual JObject(Nil)
          (json \ "user")     mustEqual {
            test.toJson(SimpleUser.externalFormat)
          }
          (json \ "password") mustEqual JNothing
      })
    }

    "return error given incorrect username" in initialized {
      // Log in with their password:
      doPost("/api/session/v1") {
        ("username" -> "mctest") ~
        ("password" -> "topsecret")
      } must beProblem {
        Client.loginUsernameIncorrect
      }
    }

    "return error given incorrect username" in initialized {
      // Log in with their username:
      doPost("/api/session/v1") {
        ("username" -> "test") ~
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
        ("username" -> "test")
      } must beProblem {
        Client.missing("password")
      }
    }
  }

  "GET /api/session/v1/'id (read session, aka re-authenticate)" should {
    "return a preexisting session" in initialized {
      val sessionId = login("test", "topsecret")

      doGet("/api/session/v1/%s".format(sessionId), auth(client(service), sessionId)) must beOk {
        ("typename" -> "session") ~
        ("id" -> sessionId.toString) ~
        ("session" -> JObject.empty) ~
        ("user" -> {
          test.toJson(SimpleUser.externalFormat)
        })
      }
    }
  }
}
