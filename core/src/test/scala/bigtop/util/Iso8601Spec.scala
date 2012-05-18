package bigtop
package util

import org.joda.time._
import org.joda.time.format._
import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._
import scalaz._
import scalaz.syntax.validation._

class Iso8601Spec extends Specification {
  "Iso8601Format.read" should {
    "parse GMT times with milliseconds in them" in {
      val actual   = Iso8601Format.read("2012-01-02T03:04:05.006Z").toOption.get
      val expected = new DateTime(2012, 01, 02, 03, 04, 05, 006, DateTimeZone.UTC)
      actual.getYear           mustEqual expected.getYear
      actual.getMonthOfYear    mustEqual expected.getMonthOfYear
      actual.getDayOfMonth     mustEqual expected.getDayOfMonth
      actual.getHourOfDay      mustEqual expected.getHourOfDay
      actual.getMinuteOfHour   mustEqual expected.getMinuteOfHour
      actual.getSecondOfMinute mustEqual expected.getSecondOfMinute
      actual.getMillisOfSecond mustEqual expected.getMillisOfSecond
      actual.getZone           mustEqual expected.getZone
    }

    "parse GMT times with no milliseconds in them" in {
      val actual   = Iso8601Format.read("2012-01-02T03:04:05Z").toOption.get
      val expected = new DateTime(2012, 01, 02, 03, 04, 05, 000, DateTimeZone.UTC)
      actual.getYear           mustEqual expected.getYear
      actual.getMonthOfYear    mustEqual expected.getMonthOfYear
      actual.getDayOfMonth     mustEqual expected.getDayOfMonth
      actual.getHourOfDay      mustEqual expected.getHourOfDay
      actual.getMinuteOfHour   mustEqual expected.getMinuteOfHour
      actual.getSecondOfMinute mustEqual expected.getSecondOfMinute
      actual.getMillisOfSecond mustEqual expected.getMillisOfSecond
      actual.getZone           mustEqual expected.getZone
    }

    "reject times in other time zones" in {
      val actual   = Iso8601Format.read("2012-01-02T03:04:05+0000").toOption
      val expected = None
      actual mustEqual None
    }
  }

  "Iso8601Format.write" should {
    "write GMT times with milliseconds in them" in {
      val actual   = Iso8601Format.write(new DateTime(2012, 01, 02, 03, 04, 05, 006, DateTimeZone.UTC))
      val expected = "2012-01-02T03:04:05.006Z"
      actual mustEqual expected
    }

    "convert times in other time zones" in {
      val actual1   = Iso8601Format.write(new DateTime(2012, 01, 02, 03, 04, 05, 006, DateTimeZone.forOffsetHours(-10)))
      val expected1 = "2012-01-02T13:04:05.006Z"
      actual1 mustEqual expected1

      val actual2   = Iso8601Format.write(new DateTime(2012, 01, 02, 03, 04, 05, 006, DateTimeZone.forOffsetHours(10)))
      val expected2 = "2012-01-01T17:04:05.006Z"
      actual1 mustEqual expected1
    }
  }

  "round-trip" in {
    val time = new DateTime
    Iso8601Format.read(Iso8601Format.write(time)).toOption must beSome(time.withZone(DateTimeZone.UTC))
  }
}
