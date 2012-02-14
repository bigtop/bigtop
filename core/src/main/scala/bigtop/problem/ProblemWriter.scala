package bigtop
package problem

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.{JsonWriter, JsonFormatters}

trait ProblemWriters {

  def problemToJValue(problem: Problem) =
    ("typename" -> "problem") ~
    ("subtype"  -> problem.problemType.name) ~
    problem.messages.map(msg => (msg.key -> msg.value)).foldLeft(JObject.empty)(_ ~ _)

  implicit object StringProblemJsonWriter extends JsonWriter[Problem] {
    def write(in: Problem): JValue =
      problemToJValue(in)
  }

}

object ProblemWriters extends ProblemWriters
