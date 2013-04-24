package bigtop.data

// import bigtop.data.format._
import blueeyes.core.http._
import blueeyes.json._

trait ParamImplicits {
  self: format.ParamFormats with format.OptionFormats =>

  case class SymbolW(param: Symbol) {
    // def format[T](implicit inner: JsonFormat[T]): JsonFormat[T] =
    //   paramFormat(param)(inner)

    def required[T](implicit inner: DataReader[T, String]): DataReader[T, HttpRequest[_]] =
      self.paramReader(param)(self.required[String] andThen inner)

    def optional[T](implicit inner: DataReader[T, Option[String]]): DataReader[T, HttpRequest[_]] =
      self.paramReader(param)(inner)

    def optional[T](orElse: => T)(implicit inner: DataReader[T, String]): DataReader[T, HttpRequest[_]] =
      self.paramReader(param)(self.optional(inner) andThen self.default(orElse))

    // def write[T](implicit inner: JsonWriter[T]): JsonWriter[T] =
    //   paramWriter(param)(inner)
  }

  implicit def symbolToSymbolW(in: Symbol): SymbolW = SymbolW(in)
}