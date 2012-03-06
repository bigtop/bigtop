package bigtop
package user

import blueeyes.json.JsonAST.JValue
import bigtop.concurrent.FutureImplicits
import bigtop.problem.Problems._
import bigtop.util.Uuid

trait UserActions[U <: User] extends UserLogin[U]
    with UserCreate[U]
    with UserRead[U]
    with UserUpdate[U]
    with UserDelete[U]


trait UserAction[U <: User] extends UserTypes[U] with FutureImplicits {

  def core: UserCore[U]

}


trait UserCreate[U <: User] extends UserAction[U] {

  def create(data: JValue): UserValidation =
    for {
      unsavedUser <- core.serializer.read(data).fv
      savedUser   <- core.store.read(unsavedUser.id).invert.mapFailure {
        f => Client.exists("user")
      } flatMap {
        p => core.store.create(unsavedUser)
      }

    } yield savedUser

}


trait UserRead[U <: User] extends UserAction[U] {

  def read(id: Uuid): UserValidation =
    core.store.read(id)

}


trait UserUpdate[U <: User] extends UserAction[U] {

  def update(id: Uuid, data: JValue): UnitValidation =
    for {
      oldUser   <- core.store.read(id)
      newUser   <- core.serializer.update(oldUser, data).fv
      savedUser <- core.store.update(newUser)
    } yield ()

}


trait UserDelete[U <: User] extends UserAction[U] {

  def delete(id: Uuid): UnitValidation =
    core.store.delete(id)

}
