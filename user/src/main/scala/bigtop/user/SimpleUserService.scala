package bigtop
package user

import akka.dispatch.Promise
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext
import net.lag.configgy.ConfigMap

trait SimpleUserService extends UserService[SimpleUser] {

  /** For testing purposes, get hold of the userActions and sessionActions */
  def userActions = Promise[UserActions[SimpleUser]]()
  def sessionActions = Promise[SessionActions[SimpleUser]]()

  def createActions(config: ConfigMap) = {
    val sessionWriter = new SessionWriter[SimpleUser] {}
    val userActions = new SimpleUserActions(config)
    val sessionActions = new LruMapSessionActions(config, userActions)

    this.userActions.success(userActions)
    this.sessionActions.success(sessionActions)

    (sessionActions, sessionWriter, userActions)
  }

}
