package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import bigtop.concurrent._
import bigtop.problem._
import bigtop.util.Uuid
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.core.http.HttpStatusCodes._
import org.specs2.matcher.Matcher
import scalaz._
import scalaz.syntax.validation._

class SimpleUserActionsSpec extends BlueEyesServiceSpecification with ConfigurableMongo {
  import MimeTypes._

  override def configuration = """
    services {
      user {
        v1 {
          mongo {
            servers = ["localhost"]
            database = "bigtoptest"
            collection = ["users"]
          }
        }
      }
    }
  """

  val userActions = SimpleUserActions[SimpleUser](
    rootConfig.detach("services.user.v1"),
    SimpleUser.internalFormat
  )

  def await[A](f: Future[A]) =
    Await.result(f, Duration("3s"))

  def await[S](f: FutureValidation[S]) =
    Await.result(f.inner, Duration("3s"))

  "SimpleUserActions.create" should {
    "return new user given username and password" in {
      val user = SimpleUser(
        id        = Uuid.create,
        username  = "noel",
        password  = Password.fromPassword("secret"),
        forenames = "Noel",
        surname   = "Welsh",
        admin     = false
      )
      await(userActions.create(user)) match {
        case Success(created)  => created mustEqual user
        case Failure(response) => failure("did not expect failure: " + response)
      }
    }
  }
}
