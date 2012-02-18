package bigtop
package util

import org.joda.time._
import org.joda.time.format._
import bigtop.problem.{Problem, Problems}
import scalaz.Validation
import scalaz.syntax.validation._

trait Iso8601Format extends Format[String,DateTime,String] {

  /** ISO8601 date/time format. */
  val formatString = "yyyy-MM-dd'T'HH:mm:ssZ"

  /** ISO8601 date/time format. */
  val format = DateTimeFormat.forPattern(formatString)

  /** Quick ISO8601-compatible calendar-to-string conversion. */
  def write(time: DateTime): String =
    format.print(time)

  /** Quick ISO8601-compatible string-to-calendar conversion. */
  def read(str: String): Validation[String,DateTime] =
    try {
      format.parseDateTime(str).success
    } catch {
      case exn: IllegalArgumentException =>
        ("Not a valid ISO-8601 date (%s): %s".format(formatString, str)).fail
    }

}

object Iso8601Format extends Iso8601Format
