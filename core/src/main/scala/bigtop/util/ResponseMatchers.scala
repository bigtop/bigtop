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
      case HttpResponse(status, _, _, _) =>
        status.code aka "The response status" mustEqual OK
    }

  def beOk(expected: JValue): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        status.code aka "The response status" mustEqual OK
        content aka "The response content" mustEqual expected
    }

  def beOk(beExpected: Matcher[JValue]): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, Some(content), _) =>
        status.code aka "The response status" mustEqual OK
        content aka "The response content" must beExpected
    }

  def beProblem(expected: Problem): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, content, _) =>
        val expectedResponse = expected.toResponse
        status.code aka "The response status" mustEqual expectedResponse.status.code
        content aka "The response content" mustEqual expectedResponse.content
    }

}

object ResponseMatchers extends ResponseMatchers
