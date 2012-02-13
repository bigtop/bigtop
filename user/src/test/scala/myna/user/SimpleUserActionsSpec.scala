package bigtop
package user

import akka.dispatch.{Await, Future}
import akka.util.Duration
import akka.util.duration._
import bigtop.concurrent._
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.core.http.HttpStatusCodes._
import org.specs2.matcher.Matcher
import scalaz._
import scalaz.syntax.validation._

class SimpleUserActionsSpec extends BlueEyesServiceSpecification
  with ConfigurableMongo {

  import MimeTypes._
  import ProblemWriters._

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

  lazy val mongoConfig = rootConfig.configMap("services.user.v1.mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database("user")

  val userConfig = SimpleUserConfig(mongoConfig, mongoFacade)
  val userActions = new SimpleUserActions(userConfig)

  def await[A](f: Future[A]) =
    Await.result(f, Duration("3s"))

  def await[F, S](f: FutureValidation[F, S]) =
    Await.result(f.inner, Duration("3s"))

  "SimpleUserActions.create" should {
    "return new user given username and password" in {
      await(userActions.create(("username" -> "noel") ~ ("password" -> "secret"))) match {
        case Success(user)     => user.username mustEqual "noel"
        case Failure(response) => failure("did not expect failure: " + response)
      }
    }

    "return error given no username" in {
      await(userActions.create(("password" -> "secret"))) match {
        case Success(user)    => failure("did not expect success: " + user)
        case Failure(problem) => problem mustEqual Request.NoUser
      }
    }

    "return error given no password" in {
      await(userActions.create(("username" -> "noel"))) match {
        case Success(user)    => failure("did not expect success: " + user)
        case Failure(problem) => problem mustEqual Request.NoPassword
      }
    }
  }

}
