package bigtop
package util

import org.joda.time._
import org.joda.time.format._
import bigtop.problem.{Problem, Problems}
import scalaz.Validation
import scalaz.syntax.validation._

trait Iso8601Format extends Format[String,DateTime,String] {

  /** ISO8601 date/time format. */
  val millisFormatString = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  val secondsFormatString = "yyyy-MM-dd'T'HH:mm:ssZ"

  /** ISO8601 date/time format. */
  val millisFormat = DateTimeFormat.forPattern(millisFormatString)
  val secondsFormat = DateTimeFormat.forPattern(secondsFormatString)

  /** Quick ISO8601-compatible timestamp-to-string conversion. */
  def write(time: DateTime): String =
    millisFormat.print(time)

  /** Quick ISO8601-compatible string-to-timestamp conversion. */
  def read(str: String): Validation[String,DateTime] =
    try {
      millisFormat.parseDateTime(str).success
    } catch {
      case exn: IllegalArgumentException =>
        try {
          secondsFormat.parseDateTime(str).success
        } catch {
          case exn: IllegalArgumentException =>
            ("Not a valid ISO-8601 date (%s): %s".format(millisFormatString, str)).fail
        }
    }

}

object Iso8601Format extends Iso8601Format
