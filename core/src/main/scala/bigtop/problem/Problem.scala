package bigtop
package problem

import blueeyes.core.http._
import blueeyes.json.JsonAST._
import bigtop.util.Writer
import bigtop.json.JsonWriter
import scalaz._
import scalaz.Scalaz._

case class Problem(
    val problemType: Problem.ProblemType,
    val messages: Seq[Problem.Message] = Seq()
  ) {

  import Problem._

  def ++(that: Problem): Problem =
    Problem(this.problemType + that.problemType, this.messages ++ that.messages)

  def +(messageType: String): Problem =
    this + Message(messageType)

  def +(messageType: String, args: (String, String) *): Problem =
    this + Message(messageType, args)

  def +(msg: Problem.Message): Problem =
    this.copy(messages =this.messages ++ Seq(msg))

  def toJson(implicit writer: JsonWriter[Problem]): JValue =
    writer.write(this)

  def toResponse(implicit writer: JsonWriter[Problem]): HttpResponse[JValue] =
    HttpResponse[JValue](status = this.problemType.status, content = Some(this.toJson))
}

object Problem extends ProblemImplicits {
  trait ProblemType {
    val name: String
    val status: HttpStatusCode
    def +(that: ProblemType): ProblemType
  }

  object ProblemType {
    case object Client extends ProblemType {
      val name = "client"
      val status = HttpStatusCodes.BadRequest
      def +(that: ProblemType) = that // server problems outweigh client ones
    }

    case object Server extends ProblemType {
      val name = "server"
      val status = HttpStatusCodes.InternalServerError
      def +(that: ProblemType) = this // server problems outweigh client ones
    }
  }

  val wildcardKey = "*"

  case class Message(val messageType: String, val args: Seq[(String, String)] = Seq())

}

object ServerProblem extends Problem(Problem.ProblemType.Server)
object ClientProblem extends Problem(Problem.ProblemType.Client)

trait ProblemImplicits {
  import Problem._

  // implicit def stringToMessage(messageType: String) =
  //   Message(messageType, Seq())

  implicit def messageToProblem(msg: Message) =
    ClientProblem + msg

  implicit def ProblemTypeSemigroup =
    new Semigroup[ProblemType] {
      def append(a: ProblemType, b: => ProblemType): ProblemType = a + b
    }

  implicit def ProblemSemigroup =
    new Semigroup[Problem] {
      def append(a: Problem, b: => Problem): Problem = a ++ b
    }
}

