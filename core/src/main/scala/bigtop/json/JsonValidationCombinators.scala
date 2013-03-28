package bigtop.json

import bigtop.concurrent._
import blueeyes.json._
import blueeyes.json.JsonAST._
import bigtop.problem._
import scalaz._
import scalaz.Scalaz._

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

  // Conversion to problems ------------

  def toClientProblem[T](inner: JsonValidation[T]): FutureValidation[T] =
    inner.fold(
      succ = { value => value.success[Problem] },
      fail = { errors => Problems.ClientValidation(errors).fail[T] }
    )

  def toServerProblem[T](inner: JsonValidation[T]): FutureValidation[T] =
    inner.fold(
      succ = { value => value.success[Problem] },
      fail = { errors => Problems.ServerValidation(errors).fail[T] }
    )

  // Other -----------------------------

  def malformed(expected: String, actual: JValue): JsonErrors = {
    import blueeyes.json.Printer._
    JsonErrors.Malformed(JPath.Identity, "expected %s, found %s".format(expected, compact(render(actual))))
  }

  def malformed(expected: String, actual: String): JsonErrors =
    malformed(expected, JString(actual))

  def prefixErrors[T](path: JPath, in: JsonValidation[T]): JsonValidation[T] =
    in match {
      case Failure(errors) => (errors prefix path).fail[T]
      case success => success
    }

}