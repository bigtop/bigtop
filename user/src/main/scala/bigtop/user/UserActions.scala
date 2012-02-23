package bigtop
package user

trait UserActions[U <: User] extends UserLogin[U]
    with UserCreate[U]
    with UserRead[U]
    with UserUpdate[U]
    with UserDelete[U]


trait UserAction[U <: User] extends UserTypes[U] {

  def core: UserCore[U]

}


trait UserCreate[U <: User] extends UserAction[U] {

  def create(data: JValue): UserValidation =
    for {
      unsavedUser <- core.serializer.read(data).fv
      savedUser   <- core.store.read(unsavedUser.username).invert.mapFailure {
        f => Client.exists("user")
      } flatMap {
        p => core.store.create(unsavedUser)
      }

    } yield savedUser

}


trait UserRead[U <: User] extends UserAction[U] {

  def read(username: String): UserValidation =
    for {
      user <- core.store.read(username)
    } yield user

}


trait UserUpdate[U <: User] extends UserAction[U] {

  def update(username: String, data: JValue): UnitValidation =
    for {
      oldUser <- core.store.read(username)
      newUser <- core.serializer.update(oldUser, data).fv
      savedUser <- core.store.update(newUser)
    } yield ()

}


trait UserDelete[U <: User] extends UserAction[U] {

  def delete(username: String): UnitValidation =
    core.store.delete(username)

}
