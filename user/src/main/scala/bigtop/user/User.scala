package bigtop
package user

import bigtop.util.Uuid

trait User {

  def id: Uuid

  def username: String

  def isPasswordOk(passwd: String): Boolean

}
