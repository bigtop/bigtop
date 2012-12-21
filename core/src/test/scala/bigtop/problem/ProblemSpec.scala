package bigtop
package problem

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._
import bigtop.json.JsonFormatters._
import blueeyes.core.http.HttpStatusCodes
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._

class ProblemSpec extends Specification {
  "Problem.status" should {
    "be set to something sensible by default" in {
      Problems.Authentication("foo").status mustEqual HttpStatusCodes.Forbidden
    }

    "be changeable to InternalServerError" in {
      Problems.Authentication("foo").status(500).status mustEqual HttpStatusCodes.InternalServerError
    }
  }

  "Problem.getStackTrace" should {
    "contain reference to the location in which the problem was created" in {
      val elem = Problems.Authentication("foo").getStackTrace.apply(2)
      elem.getFileName mustEqual "ProblemSpec.scala"
    }
  }

  "Problem.print" should {
    "produce intelligable output" in {
      var ans = ""
      var problem = Problems.Authentication("foo")
      problem.print(str => ans += str + "\n")
      ans.substring(0, ans.indexOf("  stackTrace:")) mustEqual {
        """
        |Problem: authentication (status Forbidden)
        |  timestamp: %s
        |  message: The user could not be authenticated.
        |  data: {
        |  data:   "credentials":"foo"
        |  data: }
        |
        """.trim.stripMargin.format(problem.timestamp)
      }
    }
  }

  // "Problem.and" should {
  //   "work with a string" in {
  //     val problem0 = Problems.Empty
  //     val problem1 = ClientProblem("Dave was here")
  //     val problem2 = problem1 and "Noel was here"
  //     problem2.messages mustEqual Seq(Message("Dave was here"), Message("Noel was here"))
  //   }

  //   "work with a string pair" in {
  //     val problem0 = Problems.Empty
  //     val problem1 = problem0.and("type", ("name", "Dave was here"))
  //     val problem2 = problem1.and("type", ("name", "Noel was here"))
  //     problem2.messages mustEqual Seq(
  //       Message("type", Seq("name" -> "Dave was here")),
  //       Message("type", Seq("name" -> "Noel was here"))
  //     )
  //   }

  //   "work with another problem" in {
  //     val problem0 = Problems.Empty
  //     val problem1 = ClientProblem("Dave was here")
  //     val problem2 = ClientProblem("Noel was here")
  //     (problem1 and problem2).messages mustEqual Seq(
  //       Message("Dave was here"),
  //       Message("Noel was here")
  //     )
  //   }

  //   "prefer server status to client status" in {
  //     val problem1 = ClientProblem("Dave was here")
  //     val problem2 = ServerProblem("Noel was here")
  //     (problem1 and problem1).status mustEqual HttpStatusCodes.BadRequest
  //     (problem1 and problem2).status mustEqual HttpStatusCodes.InternalServerError
  //     (problem2 and problem1).status mustEqual HttpStatusCodes.InternalServerError
  //     (problem2 and problem2).status mustEqual HttpStatusCodes.InternalServerError
  //   }
  // }

  "Problem.toResponse" should {
    "do something" in {
      val problem = Problems.Authentication("foo")
      problem.toJson mustEqual {
        ("typename" -> "problem") ~
        ("subtype" -> "authentication") ~
        ("timestamp" -> problem.timestamp.toJson) ~
        ("message" -> "The user could not be authenticated.") ~
        ("status" -> 403) ~
        ("data" -> ("credentials" -> "foo"))
      }
    }
  }

  "Problems.<foo>.{apply, unapply}" should {
    import Problems._
    "be the opposites of one another" in {
      // Database()                      must beLike({ case Database()                      => ok })
      Authentication("dave")          must beLike({ case Authentication("dave")          => ok })
      Authorization("dave", "stuff")  must beLike({ case Authorization("dave", "stuff")  => ok })
      NotFound("foo")                 must beLike({ case NotFound("foo")                 => ok })
      Exists("foo")                   must beLike({ case Exists("foo")                   => ok })
      // Missing("foo")                  must beLike({ case Missing("foo")                  => ok })
      // Malformed("foo", "bar")         must beLike({ case Malformed("foo", "bar")         => ok })
      EmptyRequest()                  must beLike({ case EmptyRequest()                  => ok })
      Unknown()                       must beLike({ case Unknown()                       => ok })
    }
  }
}
