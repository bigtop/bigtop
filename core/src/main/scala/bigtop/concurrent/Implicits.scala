package bigtop
package concurrent

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import scalaz._
import scalaz.Scalaz._

trait Implicits {

  case class WithFutureFlatMap[F, S](val inner: FutureValidation[F, S]) {

    def flatMap[T](fn: (S) => Future[T]): FutureValidation[F, T] =
      FutureValidation(inner.inner flatMap { validation => delayFailure(validation.map(fn)) })

  }

  case class WithValidationFlatMap[F, S](val inner: FutureValidation[F, S]) {

    def flatMap[G >: F, T](fn: (S) => Validation[G, T]): FutureValidation[G, T] =
      FutureValidation(inner.inner map { validation => validation.flatMap[G, T](fn) })

  }

  def delayFailure[F, S](in: Validation[F, Future[S]]): Future[Validation[F, S]] =
    in fold (
      failure = f => f.fail[S].future,
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
    FutureValidation(in.future)

}

object Implicits extends Implicits
