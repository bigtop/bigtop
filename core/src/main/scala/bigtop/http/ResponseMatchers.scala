package bigtop
package http

import bigtop.json._
import blueeyes.json.JsonAST.JValue
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes
import org.specs2.Specification
import org.specs2.matcher._
import bigtop.json.JsonFormatters._
import bigtop.problem._

trait ResponseMatchers extends MustMatchers with StandardMatchResults {

  private def beAny[T]: Matcher[T] =
    beLike[T] { case _ => ok }

  def beJsonResponse[T](beTheCode: Matcher[HttpStatusCode], beTheResult: Matcher[T])(implicit reader: JsonReader[T]) =
    beLike[HttpResponse[JValue]] {
      case HttpResponse(HttpStatus(code, _), _, Some(json), _) =>
        (code must beTheCode) and
        (json.as[T].fold(
          succ = { content => content must beTheResult },
          fail = { errors  => ko("Failed to parse JSON: " + json + "\nparse errors: " + errors) }
        ))

      case response =>
        ko("Response did not match the correct pattern: " + response)
    }

  // def beJsonResponse[T](beTheResult: Matcher[T])(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
  //   beJsonResponse(beAny[HttpStatusCode], beTheResult)

  def beProblemResponse(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beJsonResponse[Problem](beAny[HttpStatusCode], beTheProblem)

  def beOk[T](implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200(beAny[T])

  def beOk[T](expected: T)(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200[T](expected)

  def beOk[T](beTheResult: Matcher[T])(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200[T](beTheResult)

  def be200[T](implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200(beAny[T])

  def be200[T](expected: T)(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200[T](beEqualTo(expected))

  def be200[T](beTheResult: Matcher[T])(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    beJsonResponse[T](beEqualTo(HttpStatusCodes.OK), beTheResult)

  def be400(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beJsonResponse(beEqualTo(HttpStatusCodes.BadRequest), beTheProblem)

  def be403(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beJsonResponse(beEqualTo(HttpStatusCodes.Forbidden), beTheProblem)

  def be404(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beJsonResponse(beEqualTo(HttpStatusCodes.NotFound), beTheProblem)

  def be500(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beJsonResponse(beEqualTo(HttpStatusCodes.InternalServerError), beTheProblem)

  def beJsonpResponse(beTheCode: Matcher[HttpStatusCode], beTheResult: Matcher[String]) =
    beLike[HttpResponse[String]] {
      case HttpResponse(HttpStatus(code, _), _, Some(content), _) =>
        (code must beTheCode) and
        (content must beTheResult)

      case response =>
        ko("Response did not match the correct pattern: " + response)
    }

  def beJsonp200(): Matcher[HttpResponse[String]] =
    beJsonp200(beAny[String])

  def beJsonp200(expected: String): Matcher[HttpResponse[String]] =
    beJsonp200(beEqualTo(expected))

  def beJsonp200(beTheResult: Matcher[String]): Matcher[HttpResponse[String]] =
    beJsonpResponse(beEqualTo(HttpStatusCodes.OK), beTheResult)

}

object ResponseMatchers extends ResponseMatchers
