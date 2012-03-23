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

// Interface

trait SessionActions[U <: User] extends UserTypes[U] {

  def externalFormat: JsonWriter[Session[U]]

  def create(username: String, password: String): SessionValidation

  def read(id: Uuid): SessionValidation

}

// Implementaiton

case class SessionActionsBuilder[U <: User](
  val externalFormat: JsonWriter[Session[U]],
  val sessionCreate: SessionCreate[U],
  val sessionRead: SessionRead[U]
) extends SessionActions[U] with UserTypes[U] {

  def create(username: String, password: String): SessionValidation =
    sessionCreate.create(username, password)

  def read(id: Uuid): SessionValidation =
    sessionRead.read(id)

}


trait SessionAction[U <: User] extends UserTypes[U] {

  def core: SessionCore[U]

}


case class SessionCreate[U <: User](val userActions: UserActions[U], val core: SessionCore[U]) extends SessionAction[U] {

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


case class SessionRead[U <: User](val core: SessionCore[U]) extends SessionAction[U] {

  def read(id: Uuid): SessionValidation =
    core.store.read(id)

}
