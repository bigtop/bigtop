package bigtop
package user

import akka.dispatch.Promise
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext


object SimpleUserActionsFactory extends UserActionsFactory[SimpleUser] with AkkaDefaults {

  type Config = SimpleUserConfig

  def setup(context: ServiceContext) = {
    val config = SimpleUserConfig(context.config)
    Promise.successful(config)
  }

  def create(config: SimpleUserConfig) = {
    val userActions = new SimpleUserActions(config)

    userActions
  }

}

trait SimpleUserService extends UserService[SimpleUser] {

  type Config = SimpleUserConfig

  val userActionsFactory = SimpleUserActionsFactory

  implicit def writer = new SimpleUserExternalWriter {}
}

object SimpleUserService extends BlueEyesServer with SimpleUserService
