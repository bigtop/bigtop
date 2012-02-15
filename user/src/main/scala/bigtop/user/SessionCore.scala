package bigtop
package user

import bigtop.json.{JsonWriter, JsonFormatters}
import blueeyes.core.http.HttpRequest
import net.lag.logging.Logger

trait SessionCore[U <: User] extends UserTypes[U] with JsonFormatters {

  def store: SessionStore[U]

}
