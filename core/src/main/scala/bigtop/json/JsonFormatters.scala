package bigtop
package json


import blueeyes.json.JsonAST._


trait JsonFormatters {

  implicit val StringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(in: String) = JString(in)
  }

  implicit val JValueJsonWriter: JsonWriter[JValue] = new JsonWriter[JValue] {
    def write(in: JValue) = in
  }

}

object JsonFormatters extends JsonFormatters
