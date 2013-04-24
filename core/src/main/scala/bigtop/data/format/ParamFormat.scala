package bigtop.data
package format

import bigtop.concurrent._
import bigtop.json._
import bigtop.problem._
import bigtop.util._
import blueeyes.core.http._
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

trait ParamReader[T] extends DataReader[T, HttpRequest[_]] {
  def param: Symbol
  def reader: DataReader[T, Option[String]]

  def read(in: HttpRequest[_]) =
    reader.read(in.parameters.get(param)).fold(
      succ = { value => value.success[JsonErrors] },
      fail = { error => (error prefix param.name).fail[T] }
    )
}

trait ParamFormats {
  def paramReader[T](_param: Symbol)(inner: => DataReader[T, Option[String]]): ParamReader[T] =
    new ParamReader[T] {
      val param = _param
      val reader = inner
    }

  // def param[T](implicit inner: DataReader[T, String]): ParamReader[T] =
  //   paramReader(path) {
  //     inner
  //   }

  // def optionalParam[T](implicit inner: DataReader[T, String]): ParamReader[Option[T]] =
  //   paramReader(path) {
  //     optionReader(inner)
  //   }

  // def paramOption[T](default: => T)(implicit inner: DataReader[T, String]): ParamReader[T] =
  //   paramReader(path) {
  //     defaultReader(default)(inner)
  //   }
}
