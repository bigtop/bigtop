package bigtop.data

import bigtop.json._
import bigtop.util._
import blueeyes.json.JsonAST._

trait DataWriter[T, S] extends Writer[T, S]

object DataWriter {
  def apply[T, S](inner: T => S): DataWriter[T, S] =
    new DataWriter[T, S] { def write(in: T) = inner(in) }
}
