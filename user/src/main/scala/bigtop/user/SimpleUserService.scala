package bigtop
package user

import akka.dispatch.{Future,Promise}
import blueeyes.BlueEyesServer
import blueeyes.bkka.AkkaDefaults
import blueeyes.persistence.mongo.{ConfigurableMongo, Mongo, Database}
import blueeyes.core.service.ServiceContext
import blueeyes.json.JsonAST.JValue
import bigtop.util.Uuid
import org.streum.configrity.Configuration

object SimpleUserService {

  def isAdmin(operation: String): SecurityCheck[Future[JValue],SimpleUser] =
    SecurityCheck.simpleCheck(user => user.map(_.admin).getOrElse(false), operation)

  val canCreate = isAdmin("user.create")
  val canRead   = isAdmin("user.read")
  val canUpdate = isAdmin("user.update")
  val canDelete = isAdmin("user.delete")

  def services(config: Configuration) = {

    val userActions     = SimpleUserActionsBuilder(config)

    val sessionCore     = new LruMapSessionCore
    val sessionCreate   = SessionCreate[SimpleUser](userActions, sessionCore)
    val sessionRead     = SessionRead[SimpleUser](sessionCore)
    val authorizer      = SessionCookieAuthorizer[SimpleUser](sessionRead)
    val sessionActions  = SessionActionsBuilder[SimpleUser](sessionCore.externalFormat, sessionCreate, sessionRead)

    val userServices    = UserServicesBuilder(userActions, canCreate, canRead, canUpdate, canDelete, authorizer)
    val sessionServices = SessionServicesBuilder(sessionActions, authorizer)

    (sessionServices.service ~ userServices.service, authorizer, userActions, sessionActions)
  }

}


class LruMapSessionCore extends SessionCore[SimpleUser] {

  val store = new LruMapSessionStore[SimpleUser]

  val externalFormat = new SessionWriter[SimpleUser] {}

}
