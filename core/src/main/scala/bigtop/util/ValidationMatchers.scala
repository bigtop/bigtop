package bigtop
package util

import org.specs2.matcher.{Matcher, StandardMatchResults, MustMatchers}
import scalaz.{Success, Failure, Validation}

trait ValidationMatchers extends MustMatchers with StandardMatchResults {

  /** Use with whenDelivered */
  def beSuccess[A](matcher: Matcher[A]): Matcher[Validation[_, A]] =
    beLike {
      case Success(a) => a must matcher
    }

  def beSuccess(): Matcher[Validation[_,_]] =
    beLike {
      case Success(_) => ok
    }

  def beFailure[A](matcher: Matcher[A]): Matcher[Validation[A, _]] =
    beLike {
      case Failure(a) => a must matcher
    }

  def beFailure(): Matcher[Validation[_,_]] =
    beLike {
      case Failure(_) => ok
    }

}

object ValidationMatchers extends ValidationMatchers
