package bigtop
package session


import akka.dispatch.{Future, Promise}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.user.{UserActions, User}
import bigtop.util.Uuid
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import scala.collection.mutable.HashMap


trait SessionActions[U <: User] extends SessionTypes[U] with JsonFormatters {

  def userActions: UserActions[U]

  val map = new SynchronizedLruMap[Uuid, Session[U]](16384)

  def create(username: String, password: String): SessionValidation =
    for {
      user <- userActions.login(username, password)
    } yield {
      val key = Uuid.create()
      val session = Session(key, user, new HashMap[String, JValue]())(userActions.formatter : JsonWriter[U])
      map.put(key, session)
      session
    }

  // def get(session: Uuid, key: String) {
  //   for {
  //     session <- map.get(session).toSuccess(???)
  //     data    <- session.get(key).toSuccess(???)
  //   }
  // }

}
