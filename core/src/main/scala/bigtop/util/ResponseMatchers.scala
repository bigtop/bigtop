package bigtop
package util


import blueeyes.json.JsonAST.JValue
import blueeyes.core.http.{HttpStatus, HttpResponse, MimeTypes}
import blueeyes.core.http.HttpStatusCodes._
import org.specs2.Specification
import org.specs2.matcher.{Matcher, StandardMatchResults, MustMatchers}
import bigtop.problem.{Problem, ProblemWriters}


trait ResponseMatchers extends MustMatchers with StandardMatchResults with ProblemWriters {

  def beOk: Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, _, _) =>
        if(status == HttpStatus(OK))
          ok
        else
          ko
    }

  def beBadRequest(expected: Problem[String]): Matcher[HttpResponse[JValue]] =
    beLike {
      case HttpResponse(status, _, content, _) =>
        val expectedResponse = expected.toResponse[JValue]
        status  mustEqual expectedResponse.status
        content mustEqual expectedResponse.content
    }

}

object ResponseMatchers extends ResponseMatchers
