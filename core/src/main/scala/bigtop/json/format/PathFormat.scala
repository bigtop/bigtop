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

trait PathFormat[T] extends PathReader[T] with PathWriter[T] with JsonFormat[T]

trait PathReader[T] extends JsonReader[T] {
  val path: JPath
  val reader: JsonReader[T]

    def read(in: JValue) =
      path.extract(in) match {
        case JNothing => missing(path).fail[T]
        case other    => reader.read(other) match {
                           case Failure(errors) => (errors prefix path).fail[T]
                           case success => success
                         }
      }
}

trait PathWriter[T] extends JsonWriter[T] {
  val path: JPath
  val writer: JsonWriter[T]

  def write(in: T) =
    path.nodes.foldRight(writer.write(in)) { (node, accum) =>
      node match {
        case JPathField(name)  =>
          (name -> accum)

        case JPathIndex(index) =>
          for {
            i <- (0 to index).toList
          } yield if(i == index) accum else JNothing
      }
    }
}

trait PathFormats {
  def pathFormat[T : JsonFormat](path: JPath): JsonFormat[T] = {
    val _path = path
    val _format = implicitly[JsonFormat[T]]
    new PathFormat[T] {
      val path   = _path
      val reader = _format
      val writer = _format
    }
  }

  def pathReader[T : JsonReader](path: JPath): JsonReader[T] = {
    val _path = path
    new PathReader[T] {
      val path   = _path
      val reader = implicitly[JsonReader[T]]
    }
  }

  def pathWriter[T : JsonWriter](path: JPath): JsonWriter[T] = {
    val _path = path
    new PathWriter[T] {
      val path   = _path
      val writer = implicitly[JsonWriter[T]]
    }
  }
}
