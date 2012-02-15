package bigtop
package user

trait User {

  def username: String

  def isPasswordOk(passwd: String): Boolean

}
