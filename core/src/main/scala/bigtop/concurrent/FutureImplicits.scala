package bigtop
package concurrent

import akka.actor.ActorSystem
import akka.dispatch.{Await, Future, Promise}
import akka.util.{Duration, Timeout}
import akka.util.duration._
import bigtop.problem._
import blueeyes.bkka.AkkaDefaults
import scalaz._
import scalaz.Scalaz._

trait FutureImplicits extends AkkaDefaults {

  class FutureW[A](inner: Future[A]) {
    implicit val defaultDuration = Duration("3s")

    def await: A = await()

    def await(duration: Int): A =
      awaitInternal(duration.milliseconds)

    def await(duration: Duration = defaultDuration): A =
      awaitInternal(duration)

    // This internal method exists so we can override it in Futurevalidation without creating
    // syntax errors due to the ambiguity between the argumentless an argumented forms of "await".
    def awaitInternal(duration: Duration): A =
      Await.result(inner, duration)

    def awaitOption: Option[A] = awaitOption()

    def awaitOption(duration: Duration = defaultDuration): Option[A] =
      try {
        Some(await(duration))
      } catch {
        case exn: Exception => None
      }

    def onTimeout(system: ActorSystem, duration: Duration = defaultDuration)(default: => A): Future[A] = {
      val orElse = Promise()
      val timer  = system.scheduler.scheduleOnce(duration)(orElse.complete _)

      val ifThen = inner.map { result =>
        timer.cancel
        result
      }

      Future.firstCompletedOf(Seq(ifThen, orElse))
    }
  }

  class FutureValidationW[S](val fv: FutureValidation[S]) extends FutureW(fv.inner) {
    def awaitSuccess: S = awaitSuccess()

    def awaitSuccess(duration: Duration = defaultDuration): S =
      await(duration) match {
        case Success(success) => success
        case Failure(failure) => throw new Exception("awaitSuccess received failure: " + failure)
      }

    override def awaitInternal(duration: Duration): Validation[Problem, S] =
      try {
        super.awaitInternal(duration)
      } catch { case exn =>
        Problems.Unknown(cause = Some(exn)).fail[S]
      }
  }

  def delayFailure[S](in: Validation[Problem, Future[S]]): Future[Validation[Problem, S]] =
    in fold (
      fail = f => Promise.successful(f.fail),
      succ = s => s map (_.success)
    )

  def flattenValidations[S](in: Validation[Problem, Validation[Problem, S]]): Validation[Problem, S] =
    in fold (
      fail = f => f.fail[S],
      succ = s => s
    )

  implicit def futureOfValidationToFutureValidation[S](in: Future[Validation[Problem, S]]): FutureValidation[S] =
    FutureValidation(in)

  implicit def validationToFutureValidation[S](in: Validation[Problem, S]): FutureValidation[S] =
    FutureValidation(Promise.successful(in))

  implicit def futureValidationToFutureValidationW[S](fv: FutureValidation[S]): FutureValidationW[S] =
    new FutureValidationW(fv)

  implicit def futureToFutureW[A](f: Future[A]): FutureW[A] =
    new FutureW(f)
}

object FutureImplicits extends FutureImplicits
