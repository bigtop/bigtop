package bigtop
package user

import akka.dispatch.{Future,Promise}
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext
import blueeyes.json.JsonAST.JValue
import bigtop.util.Uuid
import net.lag.configgy.ConfigMap

object SimpleUserService {

  def services(config: ConfigMap) = {

    val sessionCore     = new LruMapSessionCore
    val sessionRead     = SimpleUserSessionRead(sessionCore)
    val authorizer      = SimpleUserAuthorizer(sessionRead)

    val userActions     = new SimpleUserActions(config)
    val sessionActions  = SimpleUserSessionActions(sessionCore, userActions, sessionRead)

    val userServices    = SimpleUserUserServices(authorizer, userActions)
    val sessionServices = new SimpleUserSessionServices(authorizer, sessionActions)

    (sessionServices.service ~ userServices.service, authorizer, userActions, sessionActions)
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

case class SimpleUserSessionActions(val core: SessionCore[SimpleUser],
                                    val userActions: UserActions[SimpleUser],
                                    val sessionRead: SessionRead[SimpleUser])
  extends SessionActions[SimpleUser]
{
  override def read(id: Uuid) = sessionRead.read(id)
}


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

case class SimpleUserSessionServices(val authorizer: Authorizer[SimpleUser],
                                     val action: SessionActions[SimpleUser])
  extends SessionServices[SimpleUser]
  with SessionCreateService[SimpleUser]
  with SessionReadService[SimpleUser]
