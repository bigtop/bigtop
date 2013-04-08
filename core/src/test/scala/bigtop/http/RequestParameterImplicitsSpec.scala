package bigtop.http

import org.specs2.mutable.Specification
import bigtop.http.RequestParameterImplicits._
import bigtop.json._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

class RequestParameterImplicitsSpec extends Specification {

  val request = HttpRequest[ByteChunk](
    HttpMethods.GET,
    URI("http://example.com/foo/bar/baz?num=123&str=abc&bool=true")
  )

  "request.mandatoryParam" should {
    "succeed in normal circumstances" in {
      httpRequestToHttpRequestW(request).mandatoryParam[Int]('num) mustEqual Success(123)
    }

    "fail if a type conversion is impossible" in {
      request.mandatoryParam[Int]('str) mustEqual Failure(JsonErrors.Malformed("str", "expected int, found 'abc'"))
    }

    "fail if the parameter is missing" in {
      request.mandatoryParam[Int]('foo) mustEqual Failure(JsonErrors.Missing("foo"))
    }
  }

  "request.optionalParam - single argument" should {
    "succeed in normal circumstances" in {
      request.optionalParam[Int]('num) mustEqual Success(Some(123))
    }

    "fail if a type conversion is impossible" in {
      request.optionalParam[Int]('str) mustEqual Failure(JsonErrors.Malformed("str", "expected int, found 'abc'"))
    }

    "succeed if the path is missing" in {
      request.optionalParam[Int]('foo) mustEqual Success(Option.empty[Int])
    }
  }

  "request.optionalParam - two arguments" should {
    "succeed in normal circumstances" in {
      request.optionalParam[Int]('num, -1) mustEqual Success(123)
    }

    "fail if a type conversion is impossible" in {
      request.optionalParam[Int]('str, -1) mustEqual Failure(JsonErrors.Malformed("str", "expected int, found 'abc'"))
    }

    "succeed if the path is missing" in {
      request.optionalParam[Int]('foo, -1) mustEqual Success(-1)
    }
  }

  "tuple" should {
    "succeed with 2 arguments" in {
      (request.mandatoryParam[Int]('num) tuple request.mandatoryParam[String]('str)) mustEqual Success((123, "abc"))
      (request.mandatoryParam[Int]('foo) tuple request.mandatoryParam[String]('str)) mustEqual Failure(JsonErrors(JsonError.Missing("foo")))
      (request.mandatoryParam[Int]('num) tuple request.mandatoryParam[String]('bar)) mustEqual Failure(JsonErrors(JsonError.Missing("bar")))
      (request.mandatoryParam[Int]('foo) tuple request.mandatoryParam[String]('bar)) mustEqual Failure(JsonErrors(JsonError.Missing("foo"), JsonError.Missing("bar")))
    }

    "succeed with 3 arguments" in {
      tuple(request.mandatoryParam[Int]('num), request.mandatoryParam[String]('str), request.mandatoryParam[Boolean]('bool)) mustEqual Success((123, "abc", true))
      tuple(request.mandatoryParam[Int]('foo), request.mandatoryParam[String]('bar), request.mandatoryParam[Boolean]('baz)) mustEqual Failure(
        JsonErrors(
          JsonError.Missing("foo"),
          JsonError.Missing("bar"),
          JsonError.Missing("baz")
        )
      )
    }
  }
}