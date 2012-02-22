package bigtop
package user

import bigtop.problem.Problem
import bigtop.json.{JsonFormat, JsonWriter, JsonFormatters}
import blueeyes.core.http.HttpRequest
import net.lag.logging.Logger

trait SessionCore[U <: User] extends UserTypes[U] with JsonFormatters {

  def store: SessionStore[U]

  implicit def externalFormat: JsonWriter[Session[U]]

}
