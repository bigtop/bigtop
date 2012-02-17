package bigtop
package user

import akka.util.Timeout
import bigtop.util.{ResponseMatchers, ValidationMatchers}
import bigtop.concurrent.FutureImplicits
import blueeyes.core.service.test.BlueEyesServiceSpecification
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.concurrent.test.FutureMatchers
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.data._

trait UserServiceSpecification extends BlueEyesServiceSpecification
    with ConfigurableMongo
    with ResponseMatchers
    with ValidationMatchers
    with FutureImplicits
    with FutureMatchers
    with BijectionsChunkJson
{

  private implicit def timeout = Timeout(3000)

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

  val testUser: JObject = ("username" -> "dave") ~ ("password" -> "supersecret")
  val testUserId = "dave"

  def createUser(username: String, password: String) = {
    val body: JValue = ("username" -> username) ~ ("password" -> password)
    service.contentType[JValue](application/json).post("/api/user/v1")(body)
  }

  def initialize() {
    database(remove.from("users"))
    createUser("dave", "supersecret")
  }

  def initialized[T](f: => T) = {
    initialize()
    f
  }

}
