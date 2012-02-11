package bigtop
package problem


import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.json.{JsonWriter, JsonFormatters}


trait ProblemWriters {

  def problemToJValue[A](in: Problem[A])(implicit w: JsonWriter[A]) =
    in match {
      case BadRequest(msg, _) =>
        ("typename" -> "problem") ~
        ("subtype"  -> "badrequest") ~
        ("message"  -> w.write(msg))

      case InternalError(msg, _) =>
        ("typename" -> "problem") ~
        ("subtype"  -> "internalerror") ~
        ("message"  -> w.write(msg))
    }


  implicit object StringProblemJsonWriter extends JsonWriter[Problem[String]]
  {
    def write(in: Problem[String]): JValue =
      problemToJValue[String](in)(JsonFormatters.StringJsonWriter)
  }

}

object ProblemWriters extends ProblemWriters
