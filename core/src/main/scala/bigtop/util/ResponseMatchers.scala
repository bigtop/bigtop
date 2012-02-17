package bigtop
package util

import blueeyes.json.JsonAST.JValue
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.core.http.HttpStatusCodes._
import org.specs2.Specification
import org.specs2.matcher._
import bigtop.problem.{Problem, ProblemWriters}

trait ResponseMatchers extends MustMatchers
  with StandardMatchResults
  with ProblemWriters {

  def beOk: Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        status.code aka jsonString(content) mustEqual OK
    }

  def beOk(expected: JValue): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        (jsonString(content) mustEqual jsonString(expected)) and
        (status.code mustEqual OK)
    }

  def beOk(beExpected: Matcher[JValue]): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        (content aka jsonString(content) must beExpected) and
        (status.code mustEqual OK)
    }

  def beProblem(expected: Problem): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        (jsonString(content) mustEqual jsonString(expected.toJson)) and
        (status.code mustEqual expected.status.code)
    }

  def jsonDesc(json: JValue, prefix: String = "") =
    (ignored: String) => prefix + jsonString(json)

  def jsonString(desc: String, json: JValue): String =
    desc + ": " + jsonString(json)

  def jsonString(json: JValue): String = {
    import blueeyes.json.Printer._
    compact(render(json))
  }

}

object ResponseMatchers extends ResponseMatchers
