package bigtop
package user

import akka.dispatch.{Future, Promise}
import com.weiglewilczek.slf4s.Logging
import org.streum.configrity.Configuration

class LruMapSessionActions[U <: User](val config: Configuration, val userActions: UserActions[U]) extends SessionActions[U] with Logging {

  logger.debug("session config %s".format(config))

  val core = new SessionCore[U] {

    val store = new LruMapSessionStore[U]

    val externalFormat = new SessionWriter[U] {}

  }

}
