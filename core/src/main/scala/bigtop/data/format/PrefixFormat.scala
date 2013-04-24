package bigtop.data
package format

import bigtop.concurrent._
import bigtop.problem._
import bigtop.json._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

import JsonErrors.TypeError.{ apply => typeError }
import JsonErrors.Missing.{ apply => missing }

trait PrefixFormat[A, B] extends PrefixReader[A, B] with PrefixWriter[A, B] with DataFormat[A, B]

trait PrefixReader[A, B] extends DataReader[A, B] {
  def path: JPath
  def reader: DataReader[A, B]

  def read(in: B) =
    reader.read(in).fold(
      succ = { value => value.success[JsonErrors] },
      fail = { error => (error prefix path).fail[A] }
    )
}

trait PrefixWriter[A, B] extends DataWriter[A, B] {
  def path: JPath
  def writer: DataWriter[A, B]

  def write(in: A) =
    writer.write(in)
}

trait PrefixFormats {
  def prefix[A, B](_path: JPath)(inner: => DataFormat[A, B]): DataFormat[A, B] =
    new PrefixFormat[A, B] {
      val path = _path
      val reader = inner
      val writer = inner
    }
}

trait PrefixReaders {
  def prefix[A, B](_path: JPath)(inner: => DataReader[A, B]): DataReader[A, B] =
    new PrefixReader[A, B] {
      val path = _path
      val reader = inner
    }
}

trait PrefixWriters {
  def prefix[A, B](_path: JPath)(inner: => DataWriter[A, B]): DataWriter[A, B] =
    new PrefixWriter[A, B] {
      val path = _path
      val writer = inner
    }
}
