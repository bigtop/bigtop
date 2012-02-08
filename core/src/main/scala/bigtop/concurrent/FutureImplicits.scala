package bigtop
package concurrent

import akka.dispatch.{Future, Promise}
import blueeyes.bkka.AkkaDefaults
import scalaz.Validation
import scalaz.syntax.validation._

trait FutureImplicits extends AkkaDefaults {

  case class WithFutureFlatMap[F, S](val inner: FutureValidation[F, S]) {

    def flatMap[T](fn: (S) => Future[T]): FutureValidation[F, T] =
      FutureValidation(inner.inner flatMap { validation => delayFailure(validation.map(fn)) })

  }

  case class WithValidationFlatMap[F, S](val inner: FutureValidation[F, S]) {

    def flatMap[G >: F, T](fn: (S) => Validation[G, T]): FutureValidation[G, T] =
      FutureValidation(inner.inner map { validation => validation.flatMap[T, G](fn) })

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

}

object Futuremplicits extends FutureImplicits
