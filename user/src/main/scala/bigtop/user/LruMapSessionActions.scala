package bigtop
package user

import akka.dispatch.{Future, Promise}
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger

class LruMapSessionActions[U <: User](val config: ConfigMap, val userActions: UserActions[U]) extends SessionActions[U] {
  private val log = Logger.get

  log.debug("session config %s", config)

  val core = new SessionCore[U] {

    val store = new LruMapSessionStore[U]

    val externalFormat = new SessionWriter[U] {}

  }

}
