package bigtop
package user

import blueeyes.json.JsonDSL._
import blueeyes.json.JsonAST.JValue
import blueeyes.core.data.Bijection
import blueeyes.core.http.{HttpResponse,HttpStatus,HttpStatusCodes}
import blueeyes.core.http.HttpStatusCodes._
import scalaz.{NonEmptyList,Scalaz,Semigroup}
import Scalaz._

case class Errors(errors: NonEmptyList[Error]) {

  def toResponse[T](implicit b: Bijection[JValue,T], w: Writer[Errors]): HttpResponse[T] =
    HttpResponse(status = HttpStatus(BadRequest), 
                 content = Some(b(w.write(this))))

}

trait ErrorWriter extends Writer[Error] {
  def write(error: Error) = error: JValue
}

trait ErrorsWriter extends Writer[Errors] {
  implicit val w: Writer[Error]

  def write(errors: Errors) =
    ("typename", "error") ~
    ("errors" -> errors.errors.list.map(w.write _))
}

object ErrorCode {

  val NoContent   = ("content"  -> "No content was given")
  val NoUserGiven = ("user"     -> "No username was specified")
  val NoPassword  = ("password" -> "No password was specified")
}

trait ErrorImplicits {

  implicit val errorsSemigroup: Semigroup[Errors] = new Semigroup[Errors] {
    def append(s1: Errors, s2: => Errors) =
      Errors(Semigroup.NonEmptyListSemigroup.append(s1.errors, s2.errors))
  }

  implicit def errorToErrors(error: Error) =
    Errors(nel(error))

  implicit def nonEmptyListErrorToErrors(errors: NonEmptyList[Error]) =
    Errors(errors)

  implicit val errorWriter = new ErrorWriter {}
  implicit val errorsWriter = new ErrorsWriter {
    implicit val w = errorWriter
  }
  implicit val errorNelWriter = new Writer[NonEmptyList[Error]] {
    def write(e: NonEmptyList[Error]) = errorsWriter.write(e)
  }
}

object ErrorImplicits extends ErrorImplicits

