package bigtop
package session

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

trait SessionRead[U <: User] extends SessionCore[U] {

  def readSession(id: Uuid): SessionValidation =
    sessionStore.get(id)

}
