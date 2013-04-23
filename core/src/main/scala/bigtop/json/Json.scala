package bigtop.json

import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

object Json {

  def read[T : JsonReader](in: JValue) =
    implicitly[JsonReader[T]].read(in)

  def write[T : JsonWriter](in: T) =
    implicitly[JsonWriter[T]].write(in)

  def stringify[T : JsonWriter](in: T) =
    compact(render(write(in)))

}