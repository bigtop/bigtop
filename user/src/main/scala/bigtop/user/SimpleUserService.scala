package bigtop
package user

import blueeyes.BlueEyesServer
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.concurrent.Future._
import blueeyes.core.service.HttpServiceContext

trait SimpleUserService extends UserService[SimpleUserConfig,SimpleUser] with ConfigurableMongo {

  def initialize(context: HttpServiceContext) = {
    val mongoConfig = context.config.configMap("mongo")
    val mongoFacade = mongo(mongoConfig)
    val config = SimpleUserConfig(mongoConfig, mongoFacade)
    config.future
  }

  def makeActions(config: SimpleUserConfig) =
    new SimpleUserActions(config)

  implicit def writer = new SimpleUserExternalWriter {}
}

object SimpleUserService extends BlueEyesServer with SimpleUserService
