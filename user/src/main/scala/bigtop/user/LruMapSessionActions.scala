package bigtop
package user

import com.twitter.util.{LruMap,SynchronizedLruMap}
import com.weiglewilczek.slf4s.Logging
import bigtop.json._
import bigtop.util.Uuid
import bigtop.problem._
import bigtop.concurrent._
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._

case class LruMapSessionActions[U <: User](
  val userActions: UserActions[U]
) extends SessionActions[U] with FutureImplicits {
  val map = new SynchronizedLruMap[Uuid, Session[U]](16384)

  def read(id: Uuid): SessionValidation = {
    map.get(id).toSuccess(Problems.Client.noSession).fv
  }

  def save(session: Session[U]): SessionValidation = {
    map.put(session.id, session)
    session.success.fv
  }

  def delete(id: Uuid): UnitValidation = {
    map.remove(id)
    ().success.fv
  }
}
