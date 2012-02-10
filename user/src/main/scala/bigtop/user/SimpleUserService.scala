package bigtop
package user

import akka.dispatch.Promise
import blueeyes.BlueEyesServer
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext

trait SimpleUserService extends UserService[SimpleUser]
    with ConfigurableMongo
    with UserActionsFactory[SimpleUser]
{

  type Config = SimpleUserConfig

  def setup(context: ServiceContext) = {
    val mongoConfig = context.config.configMap("mongo")
    val mongoFacade = mongo(mongoConfig)
    val config = SimpleUserConfig(mongoConfig, mongoFacade)
    Promise.successful(config)
  }

  def create(config: SimpleUserConfig) =
    new SimpleUserActions(config)

  implicit def writer = new SimpleUserExternalWriter {}
}

object SimpleUserService extends BlueEyesServer with SimpleUserService
