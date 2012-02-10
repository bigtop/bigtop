package bigtop
package user


trait UserStore[U <: User] extends UserTypes[U] {

  def get(username: String): UserValidation

  def add(user: U): UserValidation

  def delete(username: String): UnitValidation

}
