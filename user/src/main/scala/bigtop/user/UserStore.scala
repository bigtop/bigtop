package bigtop
package user

import bigtop.concurrent._
import bigtop.util.Uuid

trait UserStore[U <: User] extends UserTypes[U] {

  def create(user: U): UserValidation

  def read(id: Uuid): UserValidation

  def update(user: U): UserValidation

  def delete(id: Uuid): FutureValidation[Unit]

  def searchByUsername(username: String): UserValidation

}
