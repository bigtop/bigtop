package bigtop
package session

import scala.collection.mutable.HashMap
import com.twitter.util.LruMap
import bigtop.user.{UserActions, User}
import bigtop.util.Uuid

trait SessionAction[U <: User]{ self: UserActions[U] =>

  val map = new SynchronizedLruMap[String, JValue](16384)

  def new(username: String, password: String) =
    for {
      json <- login(username, password)
    } yield {
      val key = Uuid.create()
      map += (key, new HashMap[String, JValue]())
      key
    }

  def get(session: Uuid, key: String) {
    for {
      session <- map.get(session).toSuccess(???)
      data    <- session.get(key).toSuccess(???)
    }
  }
}
