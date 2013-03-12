package bigtop
package user

import scala.concurrent.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.util.Uuid
import blueeyes.core.service.ServiceContext
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import scala.collection.mutable.HashMap

trait SessionActions[U <: User] extends UserTypes[U] {
  def userActions: UserActions[U]

  def read(id: Uuid): SessionValidation

  def save(session: Session[U]): SessionValidation

  def delete(id: Uuid): UnitValidation

  def create(username: String, password: String): SessionValidation = {
    for {
      user  <- userActions.login(username, password)
      saved <- save(Session(Uuid.create, user, user, new HashMap[String, JValue]()))
    } yield saved
  }

  def changeIdentity(id: Uuid, user: U): SessionValidation = {
    for {
      session <- read(id)
      saved   <- save(session.copy(effectiveUser = user))
    } yield saved
  }

  def restoreIdentity(id: Uuid): SessionValidation = {
    for {
      session <- read(id)
      saved   <- save(session.copy(effectiveUser = session.realUser))
    } yield saved
  }
}
