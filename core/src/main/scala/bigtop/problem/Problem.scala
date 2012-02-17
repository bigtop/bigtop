package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json.JsonAST._
import bigtop.util.Writer
import bigtop.json.JsonWriter
import scalaz._
import scalaz.Scalaz._

sealed trait Problem extends ProblemWriters {
  val status: HttpStatusCode

  def messages: Seq[Problem.Message]

  import Problem._

  def and(that: Problem): Problem

  def and(messageType: String): Problem =
    this.and(Message(messageType))

  def and(messageType: String, args: (String, String) *): Problem =
    this.and(Message(messageType, args))

  def and(msg: Problem.Message): Problem

  def toJson: JValue =
    problemToJValue(this)

  def toResponse: HttpResponse[JValue] =
    HttpResponse[JValue](status = this.status, content = Some(this.toJson))
}

object Problem extends ProblemImplicits {
  case class Message(val messageType: String, val args: Seq[(String, String)] = Seq())
}

final case class ServerProblem(val messages: Seq[Problem.Message]) extends Problem {
  val status = HttpStatusCodes.InternalServerError

  def and(that: Problem): Problem =
    ServerProblem(this.messages ++ that.messages)

  def and(msg: Problem.Message) =
    this.copy(messages = this.messages ++ Seq(msg))
}

object ServerProblem extends ProblemImplicits {
  import Problem.Message

  def apply(msg: String, args: (String, String) *): Problem =
    apply(Message(msg, args))

  def apply(msg: Message): Problem =
    apply(Seq(msg))
}

final case class ClientProblem(val messages: Seq[Problem.Message]) extends Problem {
  val status = HttpStatusCodes.BadRequest

  def and(that: Problem): Problem =
    that match {
      case ServerProblem(_) => ServerProblem(this.messages ++ that.messages)
      case ClientProblem(_) => ClientProblem(this.messages ++ that.messages)
    }

  def and(msg: Problem.Message): Problem =
    this.copy(messages = this.messages ++ Seq(msg))
}

object ClientProblem extends ProblemImplicits {
  import Problem.Message

  def apply(msg: String, args: (String, String) *): Problem =
    apply(Message(msg, args))

  def apply(msg: Message): ClientProblem =
    apply(Seq(msg))
}

trait ProblemImplicits {
  import Problem._

  implicit def ProblemSemigroup =
    new Semigroup[Problem] {
      def append(a: Problem, b: => Problem): Problem = a and b
    }
}
