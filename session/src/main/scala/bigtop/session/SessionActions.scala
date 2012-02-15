package bigtop
package session


import akka.dispatch.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.user.{UserActions, User, UserActionsFactory}
import bigtop.util.Uuid
import blueeyes.core.service.ServiceContext
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scala.collection.mutable.HashMap


case class SessionActionsConfig[U <: User](val config: ConfigMap, val userActions: UserActions[U]) extends ConfigurableMongo {
  private val log = Logger.get

  log.debug("session config %s", config)

  lazy val mongoConfig = config.configMap("mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database(config("mongo.database"))

}

class SessionActionsFactory[U <: User](val userActionsFactory: UserActionsFactory[U]) {

  type Config = SessionActionsConfig[U]

  def setup(context: ServiceContext): Future[Config] =
    for(userConfig: userActionsFactory.Config <- userActionsFactory.setup(context)) yield {
      SessionActionsConfig(context.config, userActionsFactory.create(userConfig))
    }

  def create(config: Config): SessionActions[U] =
    new SessionActions[U](config)

}

class SessionActions[U <: User](config: SessionActionsConfig[U]) extends SessionTypes[U] with JsonFormatters {

  val map = new SynchronizedLruMap[Uuid, Session[U]](16384)

  def create(username: String, password: String): SessionValidation =
    for {
      user <- config.userActions.login(username, password)
    } yield {
      val key = Uuid.create()
      val session = Session(key, user, new HashMap[String, JValue]())(config.userActions.formatter : JsonWriter[U])
      map.put(key, session)
      session
    }

  // def get(session: Uuid, key: String) {
  //   for {
  //     session <- map.get(session).toSuccess(???)
  //     data    <- session.get(key).toSuccess(???)
  //   }
  // }

}
