package bigtop
package problem

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.{JsonWriter, JsonFormatters}

trait ProblemWriters {

  def problemToJValue(problem: Problem) =
    ("typename" -> "problem") ~
    ("subtype"  -> problem.problemType.name) ~
    ("messages" -> problem.messages.map(messageToJValue _))

  def messageToJValue(message: Problem.Message) =
    ("typename" -> message.messageType) ~
    message.args.foldLeft(JObject.empty)(_ ~ _)

  implicit object StringProblemJsonWriter extends JsonWriter[Problem] {
    def write(in: Problem): JValue =
      problemToJValue(in)
  }

}

object ProblemWriters extends ProblemWriters
