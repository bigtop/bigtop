package bigtop
package problem

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.core.http._
import bigtop.json._
import scalaz.Validation
import scalaz.syntax.validation._
import scalaz.std.list._
import scalaz.syntax.traverse._

trait ProblemFormat extends JsonFormatters {

  implicit object SeqStringStringFormat extends JsonFormat[Problem, Seq[(String, String)]] {
    def write(in: Seq[(String, String)]) =
      JObject.empty ~ in.foldLeft(JObject.empty)(_ ~ _)

    def read(in: JValue) =
      in match {
        case JObject(fields) =>
          (fields.map {
            case JField(name, JString(value)) =>
              (name -> value).success[Problem]
            case _ =>
              Problems.Client.malformed("message fields", fields.toString).fail[(String, String)]
          }).sequence[({type l[A] = Validation[Problem,A]})#l, (String,String)]
        case _ =>
          Problems.Client.malformed("message", in.toString).fail[Seq[(String,String)]]
      }
  }

  implicit object MessageFormat extends JsonFormat[Problem, Problem.Message] {
    def write(in: Problem.Message): JValue =
      ("typename" -> in.messageType) ~ in.args.foldLeft(JObject.empty)(_ ~ _)

    def read(in: JValue): Validation[Problem, Problem.Message] =
      for {
        messageType <- in.mandatory[String]("typename")
        args        <- in.as[Seq[(String, String)]].map(_.filterNot(_._1 == "typename"))
      } yield Problem.Message(messageType, args)
  }

  implicit val seqMessageFormat = JsonFormatters.buildSeqFormat[Problem.Message]

  implicit object ProblemJsonFormat extends JsonFormat[Problem, Problem] {
    def write(in: Problem): JValue =
      ("typename" -> "problem") ~
      ("subtype"  -> in.status.value) ~
      ("messages" -> in.messages.toJson)

    def read(in: JValue) = {
      import HttpStatusCodeImplicits._

      for {
        status   <- in.mandatory[Int]("subtype")
        messages <- in.mandatory[Seq[Problem.Message]]("messages")
        problem  <- {
          val code: HttpStatusCode = status
          code match {
            case client: ClientError =>
              ClientProblem(SourceLocation.Unknown, messages, Nil, code).success
            case server: ServerError =>
              ServerProblem(SourceLocation.Unknown, messages, Nil, code).success
            case _ =>
              ClientProblem(
                SourceLocation.atDepth(2),
                "subtype",
                ((code.value + ":" + code.defaultMessage) -> "This status code is not an error code")
              ).fail
          }
        }

      } yield {
        problem
      }
    }
  }

}


object ProblemFormat extends ProblemFormat
