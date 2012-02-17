package bigtop
package problem

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._
import blueeyes.core.http._

class ProblemSpec extends Specification {
  import Problem._

  "Problem.client" should {
    "have the correct type and messages" in {
      ClientProblem.problemType mustEqual ProblemType.Client
      ClientProblem.messages mustEqual Seq()
    }
  }

  "Problem.server" should {
    "have the correct type and messages" in {
      ServerProblem.problemType mustEqual ProblemType.Server
      ServerProblem.messages mustEqual Seq()
    }
  }

  "Problem.+" should {
    "work with a string" in {
      val problem0 = ClientProblem
      val problem1 = problem0 + "Dave was here"
      val problem2 = problem1 + "Noel was here"
      problem2.messages mustEqual Seq(Message("Dave was here"), Message("Noel was here"))
    }

    "work with a string pair" in {
      val problem0 = ClientProblem
      val problem1 = problem0 + ("name", ("hero" -> "Dave was here"))
      val problem2 = problem1 + ("name", ("hero" -> "Noel was here"))
      problem2.messages mustEqual Seq(Message("name", Seq(("hero" -> "Dave was here"))), Message("name", Seq(("hero" -> "Noel was here"))))
    }
  }

  "Problem.++" should {
    "append messages" in {
      val problem1 = ClientProblem + "Dave was here"
      val problem2 = ClientProblem + "Noel was here"
      (problem1 ++ problem2).messages mustEqual Seq(Message("Dave was here"), Message("Noel was here"))
    }

    "prefer server status" in {
      val problem1 = ClientProblem + "Dave was here"
      val problem2 = ServerProblem + "Noel was here"
      (problem1 ++ problem1).problemType mustEqual ProblemType.Client
      (problem1 ++ problem2).problemType mustEqual ProblemType.Server
      (problem2 ++ problem1).problemType mustEqual ProblemType.Server
      (problem2 ++ problem2).problemType mustEqual ProblemType.Server
    }
  }

  "Problem.toResponse" should {
    "do something" in { pending }
    "not mess up" in { pending }
  }

}
