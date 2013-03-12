package bigtop
package user

import scala.concurrent.{Future,Promise}
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
  val canChange = isAdmin("session.changeIdentity")

  def services(config: Configuration) = {
    val userActions    = SimpleUserActions[SimpleUser](config, SimpleUser.internalFormat)
    val sessionActions = LruMapSessionActions[SimpleUser](userActions)
    val authorizer     = SessionCookieAuthorizer[SimpleUser](sessionActions)
    val userServices   = UserServicesBuilder(
      userActions,
      canCreate,
      canRead,
      canUpdate,
      canDelete,
      authorizer,
      SimpleUser.externalFormat
    )

    val sessionServices = SessionServicesBuilder(
      sessionActions,
      userActions,
      canChange,
      authorizer,
      Session.externalFormat(SimpleUser.externalFormat)
    )

    (sessionServices.service ~ userServices.service, authorizer, userActions, sessionActions)
  }

}
