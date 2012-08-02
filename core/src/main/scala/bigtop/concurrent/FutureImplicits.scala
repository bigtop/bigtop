package bigtop
package concurrent

import akka.actor.ActorSystem
import akka.dispatch.{Await, Future, Promise}
import akka.util.{Duration, Timeout}
import bigtop.problem._
import blueeyes.bkka.AkkaDefaults
import scalaz.{Validation, Success, Failure}
import scalaz.syntax.validation._

trait FutureImplicits extends AkkaDefaults {
  type FvProblem[A] = FutureValidation[Problem, A]

  class FutureW[A](inner: Future[A]) {
    implicit val defaultDuration = Duration("3s")

    def await: A = await()

    def await(duration: Duration = defaultDuration): A =
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

  class FutureValidationW[E, S](val fv: FutureValidation[E,S]) extends FutureW(fv.inner) {
    def awaitSuccess: S = awaitSuccess()

    def awaitSuccess(duration: Duration = defaultDuration): S =
      await(duration) match {
        case Success(success) => success
        case Failure(failure) => throw new Exception("awaitSuccess received failure: " + failure)
      }
  }

  def delayFailure[F, S](in: Validation[F, Future[S]]): Future[Validation[F, S]] =
    in fold (
      failure = f => Promise.successful(f.fail[S]),
      success = s => s map (_.success[F])
    )

  def flattenValidations[F, S](in: Validation[F, Validation[F, S]]): Validation[F, S] =
    in fold (
      failure = f => f.fail[S],
      success = s => s
    )

  implicit def futureOfValidationToFutureValidation[F, S](in: Future[Validation[F, S]]): FutureValidation[F, S] =
    FutureValidation(in)

  implicit def validationToFutureValidation[F, S](in: Validation[F, S]): FutureValidation[F, S] =
    FutureValidation(Promise.successful(in))

  implicit def futureValidationToFutureValidationW[E,S](fv: FutureValidation[E,S]): FutureValidationW[E,S] =
    new FutureValidationW(fv)

  implicit def futureToFutureW[A](f: Future[A]): FutureW[A] =
    new FutureW(f)
}

object FutureImplicits extends FutureImplicits
