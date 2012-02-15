package bigtop
package user

import bigtop.user.User

trait SessionActions[U <: User] extends SessionCore[U]
    with SessionCreate[U]
    with SessionRead[U]
