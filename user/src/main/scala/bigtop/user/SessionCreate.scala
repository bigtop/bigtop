package bigtop
package user

import akka.dispatch.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.user.{UserActions, User}
import bigtop.util.Uuid
import blueeyes.core.service.ServiceContext
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scala.collection.mutable.HashMap

trait SessionCreate[U <: User] extends SessionCore[U] {

  def userActions: UserActions[U]

  def createSession(username: String, password: String): SessionValidation =
    for {
      user <- userActions.loginUser(username, password)
    } yield {
      val id = Uuid.create()
      val session = Session(id, user, new HashMap[String, JValue]())(userActions.userFormatter : JsonWriter[U])
      sessionStore.add(id, session)
      session
    }

}
