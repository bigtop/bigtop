package bigtop.json

import org.specs2.mutable.Specification
import bigtop.json.JsonFormatters._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

class JsonFormattersSpec extends Specification {

  val data =
    ("num" -> 123) ~
    ("str" -> "abc") ~
    ("obj" -> ("a" -> ("b" -> ("c" -> 123)))) ~
    ("arr" -> List(1, 2, 3))

  "json.as" should {
    "succeed in normal circumstances" in {
      JInt(123).as[Int] mustEqual Success(123)
    }

    "fail if a type conversion is impossible" in {
      JString("abc").as[Int] mustEqual Failure(JsonErrors.Malformed(".", "expected int, found \"abc\""))
    }
  }

  "json.asSeq" should {
    "succeed in normal circumstances" in {
      (List(1, 2, 3) : JValue).asSeq[Int] mustEqual Success(List(1, 2, 3))
    }

    "fail if a type conversion is impossible" in {
      JString("abc").as[Int] mustEqual Failure(JsonErrors.Malformed(".", "expected int, found \"abc\""))
    }
  }

  "json.mandatory" should {
    "succeed in normal circumstances" in {
      data.mandatory[Int]("num") mustEqual Success(123)
    }

    "succeed with full JPaths" in {
      data.mandatory[Int]("obj.a.b.c") mustEqual Success(123)
      data.mandatory[Int]("arr[1]") mustEqual Success(2)
    }

    "fail if a type conversion is impossible" in {
      data.mandatory[Int]("str") mustEqual Failure(JsonErrors.Malformed("str", "expected int, found \"abc\""))
      data.mandatory[Int]("obj.a.b") mustEqual Failure(JsonErrors.Malformed("obj.a.b", "expected int, found {\"c\":123}"))
    }

    "fail if the path is missing" in {
      data.mandatory[JValue]("foo") mustEqual Failure(JsonErrors.Missing("foo"))
      data.mandatory[Int]("obj.a.b.d") mustEqual Failure(JsonErrors.Missing("obj.a.b.d"))
    }
  }

  "json.optional - single argument" should {
    "succeed in normal circumstances" in {
      data.optional[Int]("num") mustEqual Success(Some(123))
    }

    "succeed with full JPaths" in {
      data.optional[Int]("obj.a.b.c") mustEqual Success(Some(123))
      data.optional[Int]("arr[1]") mustEqual Success(Some(2))
    }

    "fail if a type conversion is impossible" in {
      data.optional[Int]("str") mustEqual Failure(JsonErrors.Malformed("str", "expected int, found \"abc\""))
      data.optional[Int]("obj.a.b") mustEqual Failure(JsonErrors.Malformed("obj.a.b", "expected int, found {\"c\":123}"))
    }

    "succeed if the path is missing" in {
      data.optional[Int]("foo") mustEqual Success(Option.empty[Int])
      data.optional[Int]("obj.a.b.d") mustEqual Success(Option.empty[Int])
    }
  }

  "json.optional - two arguments" should {
    "succeed in normal circumstances" in {
      data.optional[Int]("num", -1) mustEqual Success(123)
    }

    "succeed with full JPaths" in {
      data.optional[Int]("obj.a.b.c", -1) mustEqual Success(123)
      data.optional[Int]("arr[1]", -1) mustEqual Success(2)
    }

    "fail if a type conversion is impossible" in {
      data.optional[Int]("str", -1) mustEqual Failure(JsonErrors.Malformed("str", "expected int, found \"abc\""))
      data.optional[Int]("obj.a.b", -1) mustEqual Failure(JsonErrors.Malformed("obj.a.b", "expected int, found {\"c\":123}"))
    }

    "succeed if the path is missing" in {
      data.optional[Int]("foo", -1) mustEqual Success(-1)
      data.optional[Int]("obj.a.b.d", -1) mustEqual Success(-1)
    }
  }

}