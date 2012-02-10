package bigtop
package error

import blueeyes.core.http.{HttpFailure, HttpResponse, HttpStatus, HttpStatusCode, HttpStatusCodes}
import blueeyes.core.http.HttpStatusCodes._
import com.weiglewilczek.slf4s.Logger
import bigtop.util.Writer

/**
 * Base class for all error types
 *
 * Records the status code and message for the response, and a message to log in the application log
 */
abstract class Problem[A,O](code: HttpStatusCode, message: A, logMessage: String) {

  def log(msg: String, logger: Logger)

  def toResponse(implicit w: Writer[A,O], logger: Logger) = {
    log(logMessage, logger)
    HttpResponse[O](status = HttpStatus(code),
                    content = Some(w.write(message)))
  }

}


/**
 * Problems resulting from bad input from the client
 */
case class BadRequest[A,O](val message: A, val logMessage: String)
    extends Problem[A,O](HttpStatusCodes.BadRequest, message, logMessage) {

  def log(msg: String, logger: Logger) =
    logger.warn(msg)

}

/**
 * Problems resulting from errors on the server
 */
case class InternalError[A,O](val message: A, val logMessage: String)
    extends Problem[A,O](HttpStatusCodes.InternalServerError, message, logMessage) {

  def log(msg: String, logger: Logger) =
    logger.error(msg)

}
