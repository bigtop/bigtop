package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.util.Writer
import bigtop.json._
import bigtop.json.JsonFormatters._
import com.weiglewilczek.slf4s.Logger
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

class Problem(
  var id: String,
  var message: String,
  var cause: Option[Throwable] = None,
  var timestamp: DateTime = new DateTime,
  var logMessage: Option[String] = None,
  var status: HttpStatusCode = HttpStatusCodes.BadRequest,
  var data: JsonConfig = JsonConfig()
) extends Throwable(message, cause getOrElse null) {
  // Helper for use in custom extractors. See Problems.scala:
  def checkId(expected: String): Option[Unit] =
    if(id == expected) Some(()) else None

  // Getters ------------------------------------

  def isClientProblem =
    this.status.isInstanceOf[ClientError]

  def isServerProblem =
    this.status.isInstanceOf[ServerError]

  // Setters ------------------------------------

  // Restrictions in Throwable mean we can't have setters for message or cause.

  def logMessage(in: String): Problem = {
    this.logMessage = Option(in)
    this
  }

  def status(in: HttpStatusCode): Problem = {
    this.status = in
    this
  }

  def status(in: Int): Problem = {
    import HttpStatusCodeImplicits._
    this.status(in : HttpStatusCode)
  }

  def data[T](key: String, value: T)(implicit writer: JsonWriter[T]): Problem = {
    this.data = this.data.set(key, value)
    this
  }

  def toResponse(implicit logger: Logger, format: JsonFormat[Problem]): HttpResponse[JValue] = {
    print(msg => logger.error(msg))
    HttpResponse[JValue](status = this.status, content = Some(this.toJson))
  }

  def print(print: (String) => Unit): Unit = {
    print("Problem: " + id + " (status " + status + ")")
    print("  timestamp: " + timestamp)
    printMessage(print, "  message: ")
    printLogMessage(print, "  log: ")
    printData(print, "  data: ")
    print("  stackTrace:")
    getStackTrace.foreach { item => print("    " + item) }
    cause.foreach { cause =>
      print("  causedBy: " + cause.getMessage)
      cause.getStackTrace.foreach { item => print("    " + item) }
    }
  }

  private def printMessage(print: String => Unit, prefix: String) =
    printLongString(message, print, prefix)

  private def printLogMessage(print: String => Unit, prefix: String) =
    logMessage.foreach(printLongString(_, print, prefix))

  private def printData(print: String => Unit, prefix: String) =
    printLongString(data.data.toString, print, prefix)

  private def printLongString(str: String, print: String => Unit, prefix: String) =
    str.split("[\r\n]").foreach(line => print(prefix + line))

  override def toString =
    "Problem(" + id + "," + message + "," + cause + "," + timestamp + "," + logMessage + "," + status + "," + data + ")"
}

object Problem {

  def apply(
    id: String,
    message: String,
    cause: Option[Throwable]   = None,
    timestamp: DateTime        = new DateTime,
    logMessage: Option[String] = None,
    status: HttpStatusCode     = HttpStatusCodes.BadRequest,
    data: JValue               = JObject.empty
  ) = new Problem(
    id         = id,
    message    = message,
    cause      = cause,
    timestamp  = timestamp,
    logMessage = logMessage,
    status     = status,
    data       = JsonConfig(data)
  )

  def unapply(in: Problem) = Some((
    in.id,
    in.message,
    in.cause,
    in.timestamp,
    in.logMessage,
    in.status,
    in.data
  ))

  implicit object problemFormat extends JsonFormat[Problem] {
    def write(in: Problem): JValue =
      ("typename"  -> "problem") ~
      ("subtype"   -> in.id) ~
      ("timestamp" -> in.timestamp.toJson) ~
      ("message"   -> in.message) ~
      ("status"    -> in.status.code.value) ~
      ("data"      -> in.data.data)

    def read(in: JValue) =
      for {
        id         <- in.mandatory[String]("subtype")
        message    <- in.optional[String]("message", "No message provided.")
        logMessage <- in.optional[String]("logMessage")
        status     <- in.optional[Int]("status", 500).map(HttpStatusCodeImplicits.int2HttpStatusCode _)
        data       <- in.optional[JValue]("data", JObject.empty)
      } yield Problem(
        id          = id,
        message     = message,
        logMessage  = logMessage,
        status      = status,
        data        = data
      )
  }

  implicit val problemSemigroup =
    new Semigroup[Problem] {
      def append(a: Problem, b: => Problem): Problem = a // and b
    }
}