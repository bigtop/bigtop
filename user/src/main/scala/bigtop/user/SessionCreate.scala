package bigtop
package user

import akka.dispatch.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.util.Uuid
import blueeyes.core.service.ServiceContext
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scala.collection.mutable.HashMap

trait SessionCreate[U <: User] extends UserTypes[U] {

  def core: SessionCore[U]

  def userActions: UserActions[U]

  def create(username: String, password: String): SessionValidation =
    for {
      user <- userActions.login(username, password)
    } yield {
      val id = Uuid.create()
      val session = Session(id, user, new HashMap[String, JValue]())(userActions.core.serializer)
      core.store.create(id, session)
      session
    }

}
