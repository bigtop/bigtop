package bigtop
package session

import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.user.{UserActions, User}
import blueeyes.core.http.HttpRequest
import net.lag.logging.Logger

trait SessionCore[U <: User] extends SessionTypes[U] with JsonFormatters {

  val sessionStore: SessionStore[U]

}
