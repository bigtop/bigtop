package bigtop
package concurrent

import akka.dispatch.{Future, Promise}
import akka.util._
import akka.util.Duration._
import bigtop.concurrent.FutureImplicits._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.bkka.AkkaDefaults
import org.specs2.mutable.Specification
import scalaz._
import scalaz.Scalaz._

class FutureValidationSpec extends Specification with AkkaDefaults {

  val data = 123.success[Problem]

  "Single FutureValidation" should {
    "return a success" in {
      Future { data }.fv.await mustEqual Success(123)
    }

    "return a failure" in {
      Future { Problems.Authentication("foo").fail }.fv.await() must beLike {
        case Failure(Problems.Authentication("foo")) => ok
      }
    }

    "yield a failure on timeout" in {
      Future { Thread.sleep(250); data }.fv.await(50) must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }

    "yield a failure when an exception is thrown" in {
      Future { throw new Exception("bar") }.fv.await() must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }
  }

  "FutureValidation.map" should {
    "return a success" in {
      Future(data).fv.map { num => num * 2 }.await mustEqual Success(246)
    }

    "yield a failure on timeout" in {
      Future(data).fv.map { num => Thread.sleep(100); num }.await(50) must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }

    "yield a failure when an exception is thrown" in {
      Future(data).fv.map { num => throw new Exception("bar") }.await() must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }
  }

  "FutureValidation.flatMap" should {
    val data = 123.success[Problem]

    "return a success" in {
      Future(data).fv.flatMap { num => (num * 2).success }.await mustEqual Success(246)
    }

    "return a failure" in {
      Future(data).fv.flatMap { num => Problems.Authentication("foo").fail[Int].fv }.await must beLike {
        case Failure(Problems.Authentication("foo")) => ok
      }
    }

    "yield a failure on timeout" in {
      Future(data).fv.flatMap { num => Thread.sleep(100); data.fv }.await(50) must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }

    "yield a failure when an exception is thrown" in {
      Future(data).fv.flatMap { num => throw new Exception("bar") }.await() must beLike {
        case Failure(Problems.Unknown()) => ok
      }
    }
  }

  "FutureValidation.fold" should {
    val data = 123.success[Problem]

    "return a success" in {
      Future(data).fv.fold(
        succ = { num => true },
        fail = { prob => false }
      ).await mustEqual true
    }

    "return a failure" in {
      Future(Problems.Authentication("foo").fail[Int]).fv.fold(
        succ = { num => true },
        fail = { prob => false }
      ).await mustEqual false
    }

    "yield a failure when an exception is thrown" in {
      Future { throw new Exception("bar") }.fv.fold(
        succ = { num => true },
        fail = { prob => false }
      ).await mustEqual false
    }

    "throw an exception on timeout" in {
      def testCode = {
        Future { Thread.sleep(100); data }.fv.fold(
          succ = { num => true },
          fail = { prob => false }
        ).await(50)
      }

      testCode must throwA[java.util.concurrent.TimeoutException]
    }
  }
}
