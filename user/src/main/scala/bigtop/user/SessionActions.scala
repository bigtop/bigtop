package bigtop
package user

trait SessionActions[U <: User] extends SessionCreate[U]
    with SessionRead[U]
