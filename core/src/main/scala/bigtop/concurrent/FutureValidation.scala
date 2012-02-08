package bigtop
package concurrent

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import scalaz._
import scalaz.Scalaz._

case class FutureValidation[F, S](val inner: Future[Validation[F, S]]) extends Implicits {
  
  def map[T](fn: (S) => T): FutureValidation[F, T] =
    FutureValidation(inner map { validation => validation map fn })
  
  def flatMap[T](fn: (S) => FutureValidation[F, T]): FutureValidation[F, T] =
    FutureValidation(
      inner flatMap { validation =>
        validation fold (
          failure = f => f.fail[T].future,
          success = s => fn(s).inner
        )
      })

  def fold[T](failure: (F) => T, success: (S) => T): Future[T] =
    inner map { validation => validation fold (failure = failure, success = success) }

  def mapFailure[G](f: F => G): FutureValidation[G, S] =
    this.inner.map(v =>
      v match {
        case Success(x) => Success(x)
        case Failure(x) => f(x).fail
      }
    )

  /** Modifier to allow use of Future's flatMap: `foo.byFuture.flatMap( ... )` */
  def byFuture: WithFutureFlatMap[F, S] =
    WithFutureFlatMap(this)
  
  /** Modifier to allow use of Validation's flatMap: `foo.byFuture.flatMap( ... )` */
  def byValidation: WithValidationFlatMap[F, S] =
    WithValidationFlatMap(this)
  
  def lift: FutureValidation[F, S] =
    this
  
  def fv: FutureValidation[F, S] =
    this

}
