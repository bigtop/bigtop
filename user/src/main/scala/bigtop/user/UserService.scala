package bigtop
package user

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service.{ServerHealthMonitorService, HttpServiceHandler,  ServiceContext, HttpService}
import blueeyes.core.data.BijectionsChunkJson
import bigtop.concurrent.FutureImplicits
import bigtop.json.JsonFormatters
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger

trait UserService[U <: User]
     extends BlueEyesServiceBuilder
     with BijectionsChunkJson
     with UserTypes[U]
{

  import FutureImplicits._
  import JsonFormatters._

  implicit def defaultTimeout = Timeout(3 seconds)

  def createActions(config: ConfigMap): (SessionActions[U], SessionWriter[U], UserActions[U])

  val userService =
    service("user", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          startup {
            Promise.successful(createActions(context.config))
          } ->
          request { actions =>
            UserServiceHandler(actions._1, actions._3)(actions._2)
          } ->
          shutdown { config =>
            Promise.successful(())
          }
        }
      }
    }

}
