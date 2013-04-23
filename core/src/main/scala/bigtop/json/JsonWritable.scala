package bigtop
package json

import bigtop.concurrent._
import bigtop.problem._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

case class JsonWritable[A](in: A) {
  def toJson(implicit w: JsonWriter[A]): JValue = w.write(in)
}

trait JsonWritableImplicits {
  implicit def writableToJsonWritable[A](in: A): JsonWritable[A] =
    JsonWritable[A](in)
}