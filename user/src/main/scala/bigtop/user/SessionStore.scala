package bigtop
package user

import bigtop.util.Uuid

trait SessionStore[U <: User] extends UserTypes[U] {

  def create(id: Uuid, session: Session[U]): SessionValidation

  def read(id: Uuid): SessionValidation

  def update(id: Uuid, session: Session[U]): SessionValidation

  def delete(id: Uuid): UnitValidation

}
