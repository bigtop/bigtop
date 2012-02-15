package bigtop
package user

import bigtop.user.User
import bigtop.util.Uuid

trait SessionStore[U <: User] extends SessionTypes[U] {

  def get(id: Uuid): SessionValidation

  def add(id: Uuid, session: Session[U]): SessionValidation

  def delete(id: Uuid): UnitValidation

}
