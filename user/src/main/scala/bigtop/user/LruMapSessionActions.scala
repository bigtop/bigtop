package bigtop
package user

import akka.dispatch.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.util.Uuid
import blueeyes.core.service.ServiceContext
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scala.collection.mutable.HashMap

class LruMapSessionActions[U <: User](val config: ConfigMap, val userActions: UserActions[U]) extends SessionActions[U] {
  private val log = Logger.get

  log.debug("session config %s", config)

  val core = new SessionCore[U] {

    val store = new LruMapSessionStore[U]

  }

}
