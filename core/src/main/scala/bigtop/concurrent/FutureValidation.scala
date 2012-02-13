package bigtop
package concurrent

import akka.dispatch.{Future, Promise}
import blueeyes.bkka.AkkaDefaults
import scalaz.Validation
import scalaz.syntax.validation._

case class FutureValidation[F, S](val inner: Future[Validation[F, S]])
    extends FutureImplicits with AkkaDefaults
{

  def map[T](fn: (S) => T): FutureValidation[F, T] =
    FutureValidation(inner map { validation => validation map fn })

  def flatMap[T](fn: (S) => FutureValidation[F, T]): FutureValidation[F, T] =
    FutureValidation(
      inner flatMap { validation =>
        validation fold (
          failure = f => Promise.successful(f.fail[T]),
          success = s => fn(s).inner
        )
      })

  def fold[T](failure: (F) => T, success: (S) => T): Future[T] =
    inner map { validation => validation fold (failure = failure, success = success) }

  def mapFailure[G](f: F => G): FutureValidation[G, S] =
    FutureValidation(
      this.fold(
        failure = f(_).fail,
        success = x => x.success
      )
    )

  /** Modifier to allow use of Future's flatMap: `foo.byFuture.flatMap( ... )` */
  def byFuture: WithFutureFlatMap[F, S] =
    WithFutureFlatMap(this)

  /** Modifier to allow use of Validation's flatMap: `foo.byFuture.flatMap( ... )` */
  def byValidation: WithValidationFlatMap[F, S] =
    WithValidationFlatMap(this)

  /** And the success shall be failures and the failures shall be successes. This is how you do logical negation */
  def invert: FutureValidation[S, F] =
    FutureValidation(
      inner map (v => v fold (
        success = s => s.fail,
        failure = f => f.success))
    )

  def lift: FutureValidation[F, S] =
    this

  def fv: FutureValidation[F, S] =
    this

}
