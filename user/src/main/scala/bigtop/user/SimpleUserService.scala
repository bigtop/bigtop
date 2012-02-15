package bigtop
package user


import akka.dispatch.Promise
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext


class SimpleUserActionsFactory extends UserActionsFactory[SimpleUser] with AkkaDefaults {

  type Config = SimpleUserActionsConfig

  def setup(context: ServiceContext) =
    Promise.successful(SimpleUserActionsConfig(context.config))

  def create(config: SimpleUserActionsConfig) =
    new SimpleUserActions(config)

}

trait SimpleUserService extends UserService[SimpleUser] {

  implicit def writer = new SimpleUserExternalWriter {}

}
