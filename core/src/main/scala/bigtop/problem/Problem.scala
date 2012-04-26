package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json.JsonAST._
import bigtop.util.Writer
import bigtop.json.JsonWriter
import com.weiglewilczek.slf4s.Logger
import scalaz._
import scalaz.Scalaz._

sealed trait Problem extends ProblemFormat {
  def status: HttpStatusCode

  def messages: Seq[Problem.Message]
  def logMessages: Seq[String]

  import Problem._

  def and(that: Problem): Problem

  def and(messageType: String): Problem =
    this.and(Message(messageType))

  def and(messageType: String, args: (String, String) *): Problem =
    this.and(Message(messageType, args))

  def and(msg: Problem.Message): Problem

  def log(msg: String): Problem

  def status(code: HttpStatusCode): Problem

  def toJson(implicit w: JsonWriter[Problem]): JValue =
    w.write(this)

  def toResponse(implicit logger: Logger): HttpResponse[JValue] = {
    logger.warn("Problem:\n  %s\n  messages: %s\n  logMessages: %s\n".format(status.toString, messages.toString, logMessages.toString))
    HttpResponse[JValue](status = this.status, content = Some(this.toJson))
  }
}

object Problem extends ProblemImplicits {
  case class Message(val messageType: String, val args: Seq[(String, String)] = Seq())
}

final case class ServerProblem(val messages: Seq[Problem.Message], val logMessages: Seq[String], val code: HttpStatusCode) extends Problem {

  def and(that: Problem): Problem =
    ServerProblem(this.messages ++ that.messages, this.logMessages ++ that.logMessages, this.status)

  def and(msg: Problem.Message) =
    this.copy(messages = this.messages ++ Seq(msg))

  def log(msg: String): Problem =
    this.copy(logMessages = msg +: this.logMessages)

  def status = code

  def status(code: HttpStatusCode) =
      this.copy(code = code)
}

object ServerProblem extends ProblemImplicits {
  import Problem.Message

  def apply(msg: String, args: (String, String) *): Problem =
    apply(Message(msg, args))

  def apply(msg: Message): Problem =
    apply(Seq(msg), Seq(), HttpStatusCodes.InternalServerError)
}

final case class ClientProblem(val messages: Seq[Problem.Message], val logMessages: Seq[String], val code: HttpStatusCode) extends Problem {

  def and(that: Problem): Problem =
    that match {
      case ServerProblem(_, _, _) => ServerProblem(this.messages ++ that.messages, this.logMessages ++ that.logMessages, this.status)
      case ClientProblem(_, _, _) => ClientProblem(this.messages ++ that.messages, this.logMessages ++ that.logMessages, that.status)
    }

  def and(msg: Problem.Message): Problem =
    this.copy(messages = this.messages ++ Seq(msg))

  def log(msg: String): Problem =
    this.copy(logMessages = msg +: this.logMessages)

  def status = code

  def status(code: HttpStatusCode) =
      this.copy(code = code)
}

object ClientProblem extends ProblemImplicits {
  import Problem.Message

  def apply(msg: String, args: (String, String) *): Problem =
    apply(Message(msg, args))

  def apply(msg: Message): ClientProblem =
    apply(Seq(msg), Seq(), HttpStatusCodes.BadRequest)
}

trait ProblemImplicits {
  import Problem._

  implicit def ProblemSemigroup =
    new Semigroup[Problem] {
      def append(a: Problem, b: => Problem): Problem = a and b
    }
}
