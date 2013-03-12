package bigtop
package concurrent

import scala.concurrent.{Future, Promise}
import scalaz._
import scalaz.Scalaz._

case class FutureValidation[F, S](val inner: Future[Validation[F, S]]) extends FutureImplicits {

  def map[T](fn: (S) => T): FutureValidation[F, T] =
    FutureValidation(inner map { validation => validation map fn })

  def flatMap[T](fn: (S) => FutureValidation[F, T]): FutureValidation[F, T] =
    FutureValidation(
      inner flatMap { validation =>
        validation fold (
          fail = f => Promise.successful(f.fail[T]),
          succ = s => fn(s).inner
        )
      })

  def fold[T](fail: (F) => T = identity[F] _, succ: (S) => T = identity[S] _): Future[T] =
    inner map { validation => validation fold (fail = fail, succ = succ) }

  def mapFailure[G](f: F => G): FutureValidation[G, S] =
    FutureValidation(
      this.fold(
        fail = f(_).fail,
        succ = x => x.success
      )
    )

  def foreach[T](f: (S) => T): Unit = {
    this.map(f)
    ()
  }

  def recover(pf: PartialFunction[Throwable, Validation[F,S]]): FutureValidation[F,S] = {
    FutureValidation[F,S](this.inner.recover(pf))
  }

  def orElse[G](f: F => FutureValidation[G, S]) =
    inner flatMap {
      (v: Validation[F, S]) => v.fold(
        succ = s => Promise.successful(s.success[G]),
        fail = f(_).inner
      )
    } fv

  /** And the success shall be failures and the failures shall be successes. This is how you do logical negation */
  def invert: FutureValidation[S, F] =
    FutureValidation(
      inner map (v => v fold (
        succ = s => s.fail,
        fail = f => f.success))
    )

  // def lift: FutureValidation[F, S] =
  //   this

  def fv: FutureValidation[F, S] =
    this
}
