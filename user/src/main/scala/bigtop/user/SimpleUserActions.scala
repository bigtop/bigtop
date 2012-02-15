package bigtop
package user


import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.problem._
import bigtop.problem.Problems._
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Validation, ValidationNEL}
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

case class SimpleUserActionsConfig(val config: ConfigMap) extends ConfigurableMongo {
  private val log = Logger.get

  log.debug("user config %s", config)

  lazy val mongoConfig = config.configMap("mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database(config("mongo.database"))

}

class SimpleUserActions(config: SimpleUserActionsConfig) extends UserActions[SimpleUser] {

  def updater = new SimpleUserExternalFormat {}
  def formatter = updater
  def store = new SimpleUserStore(config)

}

class SimpleUserStore(config: SimpleUserActionsConfig) extends UserStore[SimpleUser]
    with SimpleUserInternalReader
    with SimpleUserInternalWriter {
  import FutureImplicits._

  implicit def queryTimeout = Timeout(3.seconds)

  def collection = "users"

  def get(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      config.database(selectOne()
                      .from(collection)
                      .where("username" === username))

    user map{ u => u.toSuccess(Client.NoUser).flatMap(read _) }
  }

  def add(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      config.database(upsert(collection)
                      .set(new MongoUpdateObject(write(user)))
                      .where("username" === user.username))

    mapOrHandleError(result, (_: Unit) => user)
  }


  def delete(username: String): UnitValidation = {
    val result: Future[Unit] =
      config.database(remove.from(collection).where("username" === username))

    mapOrHandleError(result, (_: Unit) => ())
  }


  private def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[Problem,S] = {
    val ans = Promise[Validation[Problem,S]]
    f foreach { v => ans.success(mapper(v).success[Problem]) }
    f recover { case e =>
      ans.success {
        (ServerProblem + e.getMessage).fail[S]
      }
    }

    ans.fv
  }

}
