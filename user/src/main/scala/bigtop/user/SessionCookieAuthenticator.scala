package bigtop
package user

import blueeyes.core.http.HttpRequest

trait SessionCookieAuthenticator[U <: User] extends Authenticated[U] {

  def authenticated[A](request: HttpRequest[A]): Option[U]

}
