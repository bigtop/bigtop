package bigtop
package user

trait UserActions[U <: User] extends UserCore[U]
    with UserLogin[U]
    with UserCreate[U]
    with UserRead[U]
    with UserUpdate[U]
    with UserDelete[U]
