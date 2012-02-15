package bigtop
package user

trait UserStore[U <: User] extends UserTypes[U] {

  def create(user: U): UserValidation

  def read(username: String): UserValidation

  def update(user: U): UserValidation

  def delete(username: String): UnitValidation

}
