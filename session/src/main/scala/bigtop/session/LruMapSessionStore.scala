package bigtop
package session

import com.twitter.util.{LruMap,SynchronizedLruMap}
import bigtop.user.User
import bigtop.util.Uuid
import bigtop.problem._
import bigtop.concurrent._
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._

class LruMapSessionStore[U <: User] extends SessionStore[U] with FutureImplicits {

  val map = new SynchronizedLruMap[Uuid, Session[U]](16384)

  def get(id: Uuid): SessionValidation = {
    map.get(id).toSuccess(Problems.Client.NoSession).fv
  }

  def add(id: Uuid, session: Session[U]): SessionValidation = {
    map.put(id, session)
    session.success.fv
  }

  def delete(id: Uuid): UnitValidation = {
    map.remove(id)
    ().success.fv
  }


}
