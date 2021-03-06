package bigtop.json

import bigtop.concurrent._
import blueeyes.json._
import blueeyes.json.JsonAST._
import bigtop.problem._
import scalaz._
import scalaz.Scalaz._

case class JsonValidationW[T](val inner: JsonValidation[T]) {
  def toClientProblem: ProblemValidation[T] =
    inner.fold(
      succ = { value  => value.success[Problem] },
      fail = { errors => Problems.ClientValidation(errors).fail[T] }
    )

  def toServerProblem: ProblemValidation[T] =
    inner.fold(
      succ = { value  => value.success[Problem] },
      fail = { errors => Problems.ServerValidation(errors).fail[T] }
    )
}

/**
 * Methods used in JsonFormatters and RequestParameterImplicits
 */
trait JsonValidationCombinators {

  def tuple[A, B](a: => JsonValidation[A], b: => JsonValidation[B]) =
    (a |@| b).tupled

  def tuple[A, B, C](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C]) =
    (a |@| b |@| c).tupled

  def tuple[A, B, C, D](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D]) =
    (a |@| b |@| c |@| d).tupled

  def tuple[A, B, C, D, E](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E]) =
    (a |@| b |@| c |@| d |@| e).tupled

  def tuple[A, B, C, D, E, F](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E], f: => JsonValidation[F]) =
    (a |@| b |@| c |@| d |@| e |@| f).tupled

  def tuple[A, B, C, D, E, F, G](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E], f: => JsonValidation[F], g: => JsonValidation[G]) =
    (a |@| b |@| c |@| d |@| e |@| f |@| g).tupled

  def tuple[A, B, C, D, E, F, G, H](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E], f: => JsonValidation[F], g: => JsonValidation[G], h: => JsonValidation[H]) =
    (a |@| b |@| c |@| d |@| e |@| f |@| g |@| h).tupled

  def tuple[A, B, C, D, E, F, G, H, I](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E], f: => JsonValidation[F], g: => JsonValidation[G], h: => JsonValidation[H], i: => JsonValidation[I]) =
    (a |@| b |@| c |@| d |@| e |@| f |@| g |@| h |@| i).tupled

  def tuple[A, B, C, D, E, F, G, H, I, J](a: => JsonValidation[A], b: => JsonValidation[B], c: => JsonValidation[C], d: => JsonValidation[D], e: => JsonValidation[E], f: => JsonValidation[F], g: => JsonValidation[G], h: => JsonValidation[H], i: => JsonValidation[I], j: => JsonValidation[J]) =
    (a |@| b |@| c |@| d |@| e |@| f |@| g |@| h |@| i |@| j).tupled

  // Conversion to problems ------------

  implicit def jsonValidationToJsonValidationW[T](in: JsonValidation[T]): JsonValidationW[T] =
    JsonValidationW(in)

  // Other -----------------------------

  def prefixErrors[T](path: JPath)(in: JsonValidation[T]): JsonValidation[T] =
    in match {
      case Failure(errors) => (errors prefix path).fail[T]
      case success => success
    }

}