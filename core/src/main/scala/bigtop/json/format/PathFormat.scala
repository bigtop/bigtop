package bigtop.json
package format

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

import JsonErrors.TypeError.{ apply => typeError }
import JsonErrors.Missing.{ apply => missing }

trait PathFormats {
  def pathFormat[T](_path: JPath)(implicit inner: JsonFormat[T]): JsonFormat[T] =
    new PathFormat[T] {
      val path = _path
      val reader = inner
      val writer = inner
    }
}

trait PathReaders {
  def pathReader[T](_path: JPath)(implicit inner: JsonReader[T]): JsonReader[T] =
    new PathReader[T] {
      val path = _path
      val reader = inner
    }
}

trait PathWriters {
  def pathWriter[T](_path: JPath)(implicit inner: JsonWriter[T]): JsonWriter[T] =
    new PathWriter[T] {
      val path = _path
      val writer = inner
    }
}

trait PathFormat[T] extends PathReader[T] with PathWriter[T] with JsonFormat[T]

trait PathReader[T] extends JsonReader[T] {
  def path: JPath
  def reader: JsonReader[T]

  def read(in: JValue) =
    reader.read(path.extract(in)).fold(
      succ = { value => value.success[JsonErrors] },
      fail = { error => (error prefix path).fail[T] }
    )
}

trait PathWriter[T] extends JsonWriter[T] {
  def path: JPath
  def writer: JsonWriter[T]

  def write(in: T) =
    path.nodes.foldRight(writer.write(in)) { (node, accum) =>
      node match {
        case JPathField(name)  => (name -> accum)
        case JPathIndex(index) => (0 to index).toList.map(i => if(i == index) accum else JNothing)
      }
    }
}
