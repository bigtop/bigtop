package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json._
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
  var problemType: String,
  var message: String,
  var cause: Option[Throwable] = None,
  var timestamp: DateTime = new DateTime,
  var logMessage: Option[String] = None,
  var status: HttpStatusCode = HttpStatusCodes.BadRequest,
  var data: JsonConfig = JsonConfig(),
  var jsonErrors: JsonErrors = JsonErrors()
) extends Throwable(message, cause getOrElse null) {
  // Helper for use in custom extractors. See Problems.scala:
  def checkType(expected: String): Option[Unit] =
    if(problemType == expected) Some(()) else None

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

  def data[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[T] = {
    this.data.get(path)
  }

  def setData[T](path: JPath, value: T)(implicit writer: JsonWriter[T]): Problem = {
    this.data = this.data.set(path, value)
    this
  }

  def removeData[T](path: JPath): Problem = {
    this.data = this.data.remove(path)
    this
  }

  def addErrors(errors: JsonErrors) = {
    this.jsonErrors = this.jsonErrors ++ errors
    this
  }

  def errors(errors: JsonErrors) = {
    this.jsonErrors = errors
    this
  }

  def toResponse(implicit logger: Logger, format: JsonFormat[Problem]): HttpResponse[JValue] = {
    print(msg => logger.error(msg))
    HttpResponse[JValue](
      status = HttpStatus(this.status, this.message),
      content = Some(this.toJson)
    )
  }

  def print(print: (String) => Unit): Unit = {
    def printPara(prefix: String, str: String) =
      str.split("[\r\n]").foreach(line => print(prefix + line))

    print("Problem: " + problemType + " (status " + status + ")")
    print("  timestamp: " + timestamp)
    printPara("  message: ", message)
    logMessage.foreach { msg =>
      printPara("  log: ", msg)
    }
    printPara("  data: ", data.toString)
    jsonErrors.errors.foreach { error =>
      printPara("  jsonErrors: ", error.toString)
    }
    print("  stackTrace:")
    getStackTrace.foreach { item => print("    " + item) }
    cause.foreach { cause =>
      print("  causedBy: " + cause.getMessage)
      cause.getStackTrace.foreach { item => print("    " + item) }
    }
  }

  override def toString =
    "Problem(" + problemType + "," + message + "," + cause + "," + timestamp + "," + logMessage + "," + status + "," + data + "," + jsonErrors + ")"
}

object Problem {

  def apply(
    problemType: String,
    message: String,
    cause: Option[Throwable]   = None,
    timestamp: DateTime        = new DateTime,
    logMessage: Option[String] = None,
    status: HttpStatusCode     = HttpStatusCodes.BadRequest,
    data: JsonConfig           = JsonConfig(),
    jsonErrors: JsonErrors     = JsonErrors()
  ) = new Problem(
    problemType = problemType,
    message     = message,
    cause       = cause,
    timestamp   = timestamp,
    logMessage  = logMessage,
    status      = status,
    data        = data,
    jsonErrors  = jsonErrors
  )

  def unapply(in: Problem) = Some((
    in.problemType,
    in.message,
    in.cause,
    in.timestamp,
    in.logMessage,
    in.status,
    in.data,
    in.jsonErrors
  ))

  implicit object format extends JsonFormat[Problem] {
    def write(in: Problem): JValue =
      ("typename"  -> "problem") ~
      ("timestamp" -> in.timestamp.toJson) ~
      ("status"    -> in.status.code.value) ~
      ("messages"  -> {
        {
          ("typename" -> in.problemType) ~
          ("message" -> in.message) ~
          ("data" -> in.data.data)
        } :: in.jsonErrors.errors.map(_.toJson)
      })

    def read(in: JValue) =
      for {
        status      <- in.optional[Int]("status", 500).map(HttpStatusCodeImplicits.int2HttpStatusCode _)
        logMessage  <- in.optional[String]("logMessage")
        allMessages <- in.optional[JsonErrors]("messages", JsonErrors.Empty)
        (problemType, message, data, jsonErrors) <- allMessages.errors match {
                        case Nil          => (
                                               "unknown",
                                               "No messages were passed in the incoming problem.",
                                               JObject.empty,
                                               JsonErrors()
                                             ).success[JsonErrors]
                        case head :: tail => (
                                               head.errorType,
                                               head.message,
                                               head.data,
                                               JsonErrors(tail)
                                             ).success[JsonErrors]
                      }
      } yield Problem(
        problemType = problemType,
        message     = message,
        logMessage  = logMessage,
        status      = status,
        data        = JsonConfig(data),
        jsonErrors  = jsonErrors
      )
  }

  implicit object throwableWriter extends JsonWriter[Throwable] {
    def write(in: Throwable): JValue =
      ("typename"  -> "problem") ~
      ("timestamp" -> (new DateTime).toJson) ~
      ("status"    -> 500) ~
      ("messages"  -> List(
        ("typename" -> "unknown") ~
        ("message"  -> "An unknown error occurred.")
      ))
  }


  implicit val monoid = new Monoid[Problem] {
    val zero =
      Problems.Unknown(message = "", logMessage = None, cause = None)

    def append(a: Problem, b: => Problem): Problem = {
      Problems.Unknown(
        message    = "Multiple errors were encountered.",
        logMessage = a.logMessage |+| b.logMessage,
        // If we prefer a over b, we'll always get Problem.monoid.zero
        cause      = b.cause orElse a.cause,
        data       = a.data merge b.data
      )
    }
  }
}
