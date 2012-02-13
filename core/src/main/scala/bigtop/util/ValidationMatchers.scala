package bigtop
package util

import org.specs2.matcher.{Matcher, StandardMatchResults, MustMatchers}
import scalaz.{Success, Validation}


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

}

object ValidationMatchers extends ValidationMatchers
