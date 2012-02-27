package bigtop
package user

import akka.dispatch.{Future,Promise}
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext
import blueeyes.json.JsonAST.JValue
import net.lag.configgy.ConfigMap

object SimpleUserService {

  def services(config: ConfigMap) = {

    val sessionCore   = new LruMapSessionCore
    val sessionRead   = SimpleUserSessionRead(sessionCore)
    val authorizer    = SimpleUserAuthorizer(sessionRead)
    val userActions   = new SimpleUserActions(config)
    val sessionCreate = SimpleUserSessionCreate(sessionCore, userActions)

    val userServices  = SimpleUserUserServices(authorizer, userActions)
    val sessionServices = new SimpleUserSessionServices(authorizer, sessionCreate, sessionRead)

    (sessionServices.service ~ userServices.service, authorizer, userActions)
  }

}


class LruMapSessionCore extends SessionCore[SimpleUser] {

  val store = new LruMapSessionStore[SimpleUser]

  val externalFormat = new SessionWriter[SimpleUser] {}

}

case class SimpleUserSessionRead(val core: SessionCore[SimpleUser])
    extends SessionRead[SimpleUser]

case class SimpleUserAuthorizer(val action: SimpleUserSessionRead)
    extends SessionCookieAuthorizer[SimpleUser]

case class SimpleUserSessionCreate(val core: SessionCore[SimpleUser],
                                   val userActions: UserActions[SimpleUser])
    extends SessionCreate[SimpleUser]


case class SimpleUserUserServices(val authorizer: Authorizer[SimpleUser],
                              val action: UserActions[SimpleUser])
    extends UserServices[SimpleUser] {

  def isAdmin(operation: String): SecurityCheck[Future[JValue],SimpleUser] =
    SecurityCheck.simpleCheck(user => user.map(_.admin).getOrElse(false), operation)

  val canCreate = isAdmin("user.create")
  val canRead   = isAdmin("user.read")
  val canUpdate = isAdmin("user.update")
  val canDelete = isAdmin("user.delete")
}


case class SimpleUserSessionServices(val auth: Authorizer[SimpleUser],
                                     val creator: SessionCreate[SimpleUser],
                                     val reader:  SessionRead[SimpleUser])
    extends SessionServices[SimpleUser]
{

  val createService = new SessionCreateService[SimpleUser] {
    val action = creator
  }

  val readService = new SessionReadService[SimpleUser] {
    val authorizer = auth
    val action = reader
  }

  val create = createService.create
  val read   = readService.read
}
