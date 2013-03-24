package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.util.Writer
import bigtop.json._
import bigtop.json.JsonFormatters._
import com.weiglewilczek.slf4s.Logger
import scalaz._
import scalaz.Scalaz._

case class Problem(
  val id: String,
  val message: String,
  val logMessage: Option[String] = None,
  val cause: Option[Throwable] = None,
  val status: HttpStatusCode = HttpStatusCodes.BadRequest,
  val data: Map[String, String] = Map()
) extends Throwable {
  // Helper for use in custom extractors. See Problems:
  def checkId(expected: String): Option[Unit] =
    if(id == expected) Some(()) else None

  // Simpler setters for optional fields:
  def logMessage(in: String): Problem = this.copy(logMessage = Option(in))
  def cause(in: Throwable): Problem = this.copy(cause = Option(in))

  // Switch the HTTP status to 500:
  def onServer = copy(status = HttpStatusCodes.InternalServerError)

  // Switch the HTTP status to 400:
  def onClient = copy(status = HttpStatusCodes.BadRequest)

  // Convert to an HTTP response:
  def toResponse(implicit logger: Logger, format: JsonFormat[Problem, Problem]): HttpResponse[JValue] = {
    print(msg => logger.error(msg))
    HttpResponse[JValue](status = this.status, content = Some(this.toJson))
  }

  def print(print: (String) => Unit): Unit = {
    print("Problem: " + id + " (status " + status + ")")
    printMessage(print, "  message: ")
    printLogMessage(print, "  log: ")
    printData(print, "  data.")
    data.foreach { case (name, value) => printLongString(value, print, "  data: ") }
  }

  private def printMessage(print: String => Unit, prefix: String) =
    printLongString(message, print, prefix)

  private def printLogMessage(print: String => Unit, prefix: String) =
    logMessage.foreach(printLongString(_, print, prefix))

  private def printData(print: String => Unit, prefix: String) =
    data.foreach { case (name, value) => printLongString(value, print, prefix + name + ": ") }

  private def printLongString(str: String, print: String => Unit, prefix: String) =
    str.split("[\r\n]").foreach(line => print(prefix + line))
}

object Problem {
  implicit object problemDataFormat extends JsonFormat[Problem, Map[String, String]] {
    def read(in: JValue) =
      in match {
        case JObject(fields) =>
          Map(fields collect { case JField(name, JString(value)) => (name -> value) } : _*).success[Problem]

        case _ =>
          Problems.Malformed("data", "Expected a dictionary of strings.").fail[Map[String, String]]
      }

    def write(in: Map[String, String]) =
      JObject(in.toList map { case (name, value) => JField(name, JString(value))})
  }

  implicit object problemFormat extends JsonFormat[Problem, Problem] {
    def write(in: Problem): JValue =
      ("typename" -> "problem") ~
      ("subtype"  -> in.id) ~
      ("message"  -> in.message) ~
      ("status"   -> in.status.code.value) ~
      ("data"     -> problemDataFormat.write(in.data))

    def read(in: JValue) =
      for {
        id         <- in.mandatory[String]("subtype")
        message    <- in.optional[String]("message", "No message provided.")
        logMessage <- in.optional[String]("logMessage")
        status     <- in.optional[Int]("status", 500).map(HttpStatusCodeImplicits.int2HttpStatusCode _)
        data       <- in.optional[Map[String, String]]("data", Map.empty[String, String])
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