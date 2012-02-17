package bigtop
package user

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.problem._
import bigtop.problem.Problems._
import blueeyes.persistence.mongo.{ConfigurableMongo, MongoImplicits, MongoUpdateObject}
import blueeyes.json.JsonAST._
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Validation, ValidationNEL}
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

class SimpleUserActions(val config: ConfigMap) extends UserActions[SimpleUser]
    with ConfigurableMongo
{
  val self = this
  private val log = Logger.get

  log.debug("user config %s", config)

  lazy val mongoConfig = config.configMap("mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database(config("mongo.database"))

  def core = new UserCore[SimpleUser] {

    def serializer = new SimpleUserExternalFormat {}

    def store = new SimpleUserStore(self)

  }

}

class SimpleUserStore(config: SimpleUserActions) extends UserStore[SimpleUser]
    with SimpleUserInternalReader
    with SimpleUserInternalWriter
    with MongoImplicits
{
  import FutureImplicits._

  implicit def queryTimeout = Timeout(3.seconds)

  def collection = "users"

  private def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[Problem,S] = {
    val ans = Promise[Validation[Problem,S]]
    f foreach { v => ans.success(mapper(v).success[Problem]) }
    f recover { case e =>
      ans.success {
        ServerProblem(e.getMessage).fail[S]
      }
    }

    ans.fv
  }

  def create(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      config.database(insert(write(user)).into(collection))

    mapOrHandleError(result, (_: Unit) => user)
  }

  def read(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      config.database(selectOne()
                      .from(collection)
                      .where("username" === username))

    user map { u => u.toSuccess(Client.notFound("user")).flatMap(read _) }
  }


  def update(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      config.database(MongoImplicits.update(collection)
                      .set(new MongoUpdateObject(write(user)))
                      .where("username" === user.username))

    mapOrHandleError(result, (_: Unit) => user)
  }


  def delete(username: String): UnitValidation = {
    val result: Future[Unit] =
      config.database(remove.from(collection).where("username" === username))

    mapOrHandleError(result, (_: Unit) => ())
  }

}
