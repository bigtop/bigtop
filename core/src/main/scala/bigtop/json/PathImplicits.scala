package bigtop.json

import bigtop.data.{ format => data }
import bigtop.json.{ format => json }
import blueeyes.json._

trait PathFormatImplicits {
  self: json.PathFormats with json.NullableFormats =>

  val __ = JPath.Identity

  case class JPathW(path: JPath) {
    def required[T](implicit inner: JsonFormat[T]): JsonFormat[T] =
      pathFormat(path)(inner)

    def nullable[T](implicit inner: JsonFormat[T]): JsonFormat[Option[T]] =
      self.pathFormat(path)(self.nullable[T])

    def nullable[T](orElse: => T)(implicit inner: JsonFormat[T]): JsonFormat[T] =
      self.pathFormat(path)(self.nullable(orElse)(inner))
  }

  implicit def jPathTojPathW(in: JPath): JPathW = JPathW(in)
}

trait PathReaderImplicits {
  self: json.PathReaders with json.NullableReaders =>

  val __ = JPath.Identity

  case class JPathW(path: JPath) {
    def required[T](implicit inner: JsonReader[T]): JsonReader[T] =
      self.pathReader(path)(inner)

    def nullable[T](implicit inner: JsonReader[T]): JsonReader[Option[T]] =
      self.pathReader(path)(self.nullable[T](inner))

    def nullable[T](orElse: => T)(implicit inner: JsonReader[T]): JsonReader[T] =
      self.pathReader(path)(self.nullable(orElse)(inner))
  }

  implicit def jPathTojPathW(in: JPath): JPathW = JPathW(in)
}
