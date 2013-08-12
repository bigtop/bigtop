package bigtop
package concurrent

import akka.dispatch.{Future, Promise, ExecutionContext}
import blueeyes.bkka.AkkaDefaults
import bigtop.problem._
import scala.collection.generic.CanBuildFrom
import scalaz._
import scalaz.Scalaz._

case class FutureValidation[S](val inner: Future[Validation[Problem, S]]) extends FutureImplicits with AkkaDefaults {
  def map[T](fn: (S) => T): FutureValidation[T] =
    FutureValidation {
      inner map { validation =>
        validation map fn
      } recover { case exn =>
        Problems.Unknown(cause = Some(exn)).status(500).fail[T]
      }
    }

  // Needed for pattern matching in for-loops in Scala 2.9 (due to a compiler bug).
  // Don't actually use this - it's non-sensical.
  def filter(fn: S => Boolean): FutureValidation[S] =
    inner.map(value => value flatMap { value =>
      if(fn(value)) {
        value.success[Problem]
      } else Problems.Unknown(logMessage = Some("FutureValidation.filter failed.")).fail[S]
    }).fv

  def flatMap[T](fn: (S) => FutureValidation[T]): FutureValidation[T] =
    FutureValidation {
      inner flatMap { validation =>
        validation fold (
          fail = f => Promise.successful(f.fail[T]),
          succ = s => fn(s).inner
        )
      } recover { case exn =>
        Problems.Unknown(cause = Some(exn)).status(500).fail[T]
      }
    }

  def fold[T](fail: (Problem) => T = identity[Problem] _, succ: (S) => T = identity[S] _): Future[T] =
    inner map { validation =>
      validation fold (fail = fail, succ = succ)
    } recover { case exn =>
      fail(Problems.Unknown(cause = Some(exn)).status(500))
    }

  def mapFailure[G](f: Problem => Problem): FutureValidation[S] =
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

  def recover(pf: PartialFunction[Throwable, Validation[Problem, S]]): FutureValidation[S] = {
    FutureValidation[S](this.inner.recover(pf))
  }

  def orElse[G](f: Problem => FutureValidation[S]) =
    inner flatMap {
      (v: Validation[Problem, S]) => v.fold(
        succ = s => Promise.successful(s.success[Problem]),
        fail = f(_).inner
      )
    } fv

  def fv: FutureValidation[S] =
    this
}

object FutureValidation {
  def sequence[T](in: List[FutureValidation[T]])(implicit executor: ExecutionContext): FutureValidation[List[T]] = {
    FutureValidation(Future.sequence { in.map(_.inner) }.map(_.sequence[ProblemValidation, T]))
  }
}