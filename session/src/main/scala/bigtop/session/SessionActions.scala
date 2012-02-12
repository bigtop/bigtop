package bigtop
package session

import akka.dispatch.{Future, Promise}
import bigtop.json.JsonFormatters
import bigtop.user.{UserActions, User}
import bigtop.util.Uuid
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import com.twitter.util.{LruMap,SynchronizedLruMap}
import scala.collection.mutable.HashMap

trait SessionActions[U <: User] extends JsonFormatters {

  def userActions: UserActions[U]

  val map = new SynchronizedLruMap[Uuid, JValue](16384)

  def create(username: String, password: String) =
    for {
      json <- userActions.login(username, password)
    } yield {
      val key = Uuid.create()
      map.put(key, new HashMap[String, JValue]())
      key
    }

  // def get(session: Uuid, key: String) {
  //   for {
  //     session <- map.get(session).toSuccess(???)
  //     data    <- session.get(key).toSuccess(???)
  //   }
  // }

}
