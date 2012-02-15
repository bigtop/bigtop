package bigtop
package session

import bigtop.user.User

trait SessionActions[U <: User] extends SessionCore[U]
    with SessionCreate[U]
    with SessionRead[U]
