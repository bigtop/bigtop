package bigtop.json

import bigtop.data._
import bigtop.util._
import blueeyes.json.JsonAST._

trait JsonWriter[T] extends DataWriter[T, JValue]

object JsonWriter {
  def apply[T](inner: T => JValue): JsonWriter[T] =
    new JsonWriter[T] { def write(in: T) = inner(in) }
}
