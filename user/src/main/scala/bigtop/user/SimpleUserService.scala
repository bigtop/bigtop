package bigtop
package user


import akka.dispatch.Promise
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext
import net.lag.configgy.ConfigMap

trait SimpleUserService extends UserService[SimpleUser] {

  implicit def writer = new SimpleUserExternalWriter {}

  def createUserActions(config: ConfigMap) =
    new SimpleUserActions(config)

}
