package bigtop
package problem


import blueeyes.core.http.{HttpFailure, HttpResponse, HttpStatus, HttpStatusCode, HttpStatusCodes}
import blueeyes.core.http.HttpStatusCodes._
import com.weiglewilczek.slf4s.Logger
import bigtop.util.Writer
import bigtop.json.JsonWriter


/**
 * Base class for all error types
 *
 * Records the status code and message for the response, and a message to log in the application log
 */
abstract class Problem[A](code: HttpStatusCode, message: A, logMessage: Option[String] = None) {

  def log(logger: Logger)

  def toResponse[O](implicit w: Writer[Problem[A],O]) = {
//    log(logger)
    HttpResponse[O](status = HttpStatus(code),
                    content = Some(w.write(this)))
  }

}


/**
 * Problems resulting from bad input from the client
 */
case class BadRequest[A](val message: A, val logMessage: Option[String] = None)
    extends Problem[A](HttpStatusCodes.BadRequest, message, logMessage) {

  def log(logger: Logger) =
    logger.warn("A bad request was received:\n" + message +
                (logMessage map {"\n Additional detail:\n" + _} getOrElse ""))

}

/**
 * Problems resulting from errors on the server
 */
case class InternalError[A](val message: A, val logMessage: Option[String] = None)
    extends Problem[A](HttpStatusCodes.InternalServerError, message, logMessage) {

  def log(logger: Logger) =
    logger.error("An internal error occurred:\n" + message +
                (logMessage map {"\n Additional detail:\n" + _} getOrElse ""))


}
