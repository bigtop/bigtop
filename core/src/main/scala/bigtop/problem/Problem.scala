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

  def +(value: String): Problem =
    this + Message(value)

  def +(key: String, value: String): Problem =
    this + Message(key, value)

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

  case class Message(val key: String, val value: String)

  object Message {
    def apply(value: String): Message =
      Message(wildcardKey, value)
  }
}

object ServerProblem extends Problem(Problem.ProblemType.Server)
object ClientProblem extends Problem(Problem.ProblemType.Client)

trait ProblemImplicits {
  import Problem._

  implicit def stringToMessage(value: String) =
    Message(wildcardKey, value)

  implicit def stringPairToMessage(pair: (String, String)) =
    Message(pair._1, pair._2)

  implicit def ProblemTypeSemigroup =
    new Semigroup[ProblemType] {
      def append(a: ProblemType, b: => ProblemType): ProblemType = a + b
    }

  implicit def ProblemSemigroup =
    new Semigroup[Problem] {
      def append(a: Problem, b: => Problem): Problem = a ++ b
    }
}

