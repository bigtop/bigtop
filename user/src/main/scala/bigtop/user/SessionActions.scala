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
import scala.collection.mutable.HashMap

trait SessionActions[U <: User] extends SessionCreate[U]
    with SessionRead[U]

trait SessionAction[U <: User] extends UserTypes[U] {

  def core: SessionCore[U]

}

trait SessionCreate[U <: User] extends SessionAction[U] {

  def userActions: UserActions[U]

  def create(username: String, password: String): SessionValidation =
    for {
      user <- userActions.login(username, password)
    } yield {
      val id = Uuid.create()
      val session = Session(id, user, new HashMap[String, JValue]())(userActions.externalFormat)
      core.store.create(id, session)
      session
    }

}

trait SessionRead[U <: User] extends SessionAction[U] {

  def read(id: Uuid): SessionValidation =
    core.store.read(id)

}
