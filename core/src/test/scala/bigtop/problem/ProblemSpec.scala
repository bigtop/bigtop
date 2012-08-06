package bigtop
package problem

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._
import blueeyes.core.http._

class ProblemSpec extends Specification {
  import Problem._

  def loc(depth: Int) = {
    SourceLocation.atDepth(depth + 1)
  }

  "Problem.client" should {
    "have the correct status code and messages" in {
      Problems.Client.empty.status mustEqual HttpStatusCodes.BadRequest
      Problems.Client.empty.messages mustEqual Seq()
    }

    "have the correct source location" in {
      Problems.Client.empty.location mustEqual SourceLocation("ProblemSpec.scala", 22)
    }
  }

  "Problem.server" should {
    "have the correct status code and messages" in {
      Problems.Server.empty.status mustEqual HttpStatusCodes.InternalServerError
      Problems.Server.empty.messages mustEqual Seq()
    }

    "have the correct source location" in {
      Problems.Server.empty.location mustEqual SourceLocation("ProblemSpec.scala", 33)
    }
  }

  "Problem.and" should {
    "work with a string" in {
      val problem0 = Problems.Client.empty
      val problem1 = ClientProblem(loc(0), "Dave was here")
      val problem2 = problem1 and "Noel was here"
      problem2.messages mustEqual Seq(Message("Dave was here"), Message("Noel was here"))
    }

    "work with a string pair" in {
      val problem0 = Problems.Client.empty
      val problem1 = problem0.and("type", ("name", "Dave was here"))
      val problem2 = problem1.and("type", ("name", "Noel was here"))
      problem2.messages mustEqual Seq(
        Message("type", Seq("name" -> "Dave was here")),
        Message("type", Seq("name" -> "Noel was here"))
      )
    }

    "work with another problem" in {
      val problem0 = Problems.Client.empty
      val problem1 = ClientProblem(loc(0), "Dave was here")
      val problem2 = ClientProblem(loc(0), "Noel was here")
      (problem1 and problem2).messages mustEqual Seq(
        Message("Dave was here"),
        Message("Noel was here")
      )
    }

    "prefer server status to client status" in {
      val problem1 = ClientProblem(loc(0), "Dave was here")
      val problem2 = ServerProblem(loc(0), "Noel was here")
      (problem1 and problem1).status mustEqual HttpStatusCodes.BadRequest
      (problem1 and problem2).status mustEqual HttpStatusCodes.InternalServerError
      (problem2 and problem1).status mustEqual HttpStatusCodes.InternalServerError
      (problem2 and problem2).status mustEqual HttpStatusCodes.InternalServerError
    }
  }

  "Problem.status" should {
    "change the status" in {
      val problem = Problems.Server.empty
      problem.status mustEqual HttpStatusCodes.InternalServerError
      problem.status(HttpStatusCodes.BadRequest).status mustEqual HttpStatusCodes.BadRequest
    }
  }

  "Problem.toResponse" should {
    "do something" in { pending }
    "not mess up" in { pending }
  }

}
