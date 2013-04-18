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

  def beResponse[T](beTheCode: Matcher[HttpStatusCode], beTheResult: Matcher[T])(implicit reader: JsonReader[T]) =
    beLike[HttpResponse[JValue]] {
      case HttpResponse(HttpStatus(code, _), _, Some(json), _) =>
        (code must beTheCode) and
        (json.as[T].fold(
          succ = { problem => problem must beTheResult },
          fail = { errors  => ko("Failed to parse JSON: " + json + "\nparse errors: " + errors) }
        ))

      case response =>
        ko("Response did not match the correct pattern: " + response)
    }

  // def beResponse[T](beTheResult: Matcher[T])(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
  //   beResponse(beAny[HttpStatusCode], beTheResult)

  def beProblemResponse(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beResponse[Problem](beAny[HttpStatusCode], beTheProblem)

  def beOk(): Matcher[HttpResponse[JValue]] =
    be200(beAny[JValue])

  def be200(): Matcher[HttpResponse[JValue]] =
    be200(beAny[JValue])

  def be200[T](expected: T)(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    be200[T](beEqualTo(expected))

  def be200[T](beTheResult: Matcher[T])(implicit reader: JsonReader[T]): Matcher[HttpResponse[JValue]] =
    beResponse[T](beEqualTo(HttpStatusCodes.OK), beTheResult)

  def be400(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beResponse(beEqualTo(HttpStatusCodes.BadRequest), beTheProblem)

  def be403(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beResponse(beEqualTo(HttpStatusCodes.Forbidden), beTheProblem)

  def be404(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beResponse(beEqualTo(HttpStatusCodes.NotFound), beTheProblem)

  def be500(beTheProblem: Matcher[Problem]): Matcher[HttpResponse[JValue]] =
    beResponse(beEqualTo(HttpStatusCodes.InternalServerError), beTheProblem)

}

object ResponseMatchers extends ResponseMatchers
