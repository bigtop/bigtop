package bigtop
package concurrent

import akka.dispatch.{Await, Future, Promise}
import akka.util.Duration
import bigtop.problem.Problem
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

  implicit def futureValidationToFutureValidationW[E,S](fv: FutureValidation[E,S]) =
    new FutureValidationW(fv)

  implicit def futureToFutureW[A](f: Future[A]) =
    new FutureW(f)
}

object FutureImplicits extends FutureImplicits
