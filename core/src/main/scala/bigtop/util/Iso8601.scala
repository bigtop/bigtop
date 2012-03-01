package bigtop
package util

import org.joda.time._
import org.joda.time.format._
import bigtop.problem.{Problem, Problems}
import scalaz.Validation
import scalaz.syntax.validation._

trait Iso8601Format extends Format[String,DateTime,String] {

  /**
   * ISO8601 date/time format with milliseconds.
   * We always read/write UTC times, and we always put a "Z" on the end to indicate this.
   * Joda time won't parse single-letter times like this, so we use a literal "Z" instead.
   */
  val millisFormatString = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"

  /**
   * ISO8601 date/time format without milliseconds.
   * We always read/write UTC times, and we always put a "Z" on the end to indicate this.
   * Joda time won't parse single-letter times like this, so we use a literal "Z" instead.
   */
  val secondsFormatString = "yyyy-MM-dd'T'HH:mm:ss'Z'"

  /** ISO8601 date/time format. */
  val millisFormat = DateTimeFormat.forPattern(millisFormatString)
  val secondsFormat = DateTimeFormat.forPattern(secondsFormatString)

  /** Quick ISO8601-compatible timestamp-to-string conversion. */
  def write(time: DateTime): String =
    millisFormat.print(time.toDateTime(DateTimeZone.UTC))

  /** Quick ISO8601-compatible string-to-timestamp conversion. */
  def read(str: String): Validation[String,DateTime] =
    try {
      millisFormat.parseDateTime(str).withZone(DateTimeZone.UTC).success
    } catch {
      case exn: IllegalArgumentException =>
        try {
          secondsFormat.parseDateTime(str).withZone(DateTimeZone.UTC).success
        } catch {
          case exn: IllegalArgumentException =>
            ("Not a valid ISO-8601 date (%s): %s".format(millisFormatString, str)).fail
        }
    }

}

object Iso8601Format extends Iso8601Format
