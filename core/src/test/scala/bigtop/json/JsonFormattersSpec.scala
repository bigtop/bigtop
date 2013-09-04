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
    ("arr" -> List(1, 2, 3)) ~
    ("bool" -> true)

  "json.as" should {
    "succeed in normal circumstances" in {
      JInt(123).as[Int] mustEqual Success(123)
    }

    "read integers as doubles" in {
      JInt(123).as[Double] mustEqual Success(123.0)
    }

    "read doubles as integers" in {
      JDouble(123.0).as[Int] mustEqual Success(123)
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
      (List(1.0, 2.5, 3.5) : JValue).asSeq[Int] mustEqual Failure(JsonErrors(
        JsonError.Malformed("[1]", "expected int, found 2.5"),
        JsonError.Malformed("[2]", "expected int, found 3.5")
      ))
    }
  }

  "json.asMap" should {
    "succeed in normal circumstances" in {
      {
        ("a" -> 1.0) ~
        ("b" -> 2.0) ~
        ("c" -> 3.0)
      }.asMap[Int] mustEqual Success(Map("a" -> 1, "b" -> 2, "c" -> 3))
    }

    "fail if a type conversion is impossible" in {
      {
        ("a" -> 1.0) ~
        ("b" -> 2.5) ~
        ("c" -> 3.5)
      }.asMap[Int] mustEqual Failure(JsonErrors(
        JsonError.Malformed(".b", "expected int, found 2.5"),
        JsonError.Malformed(".c", "expected int, found 3.5")
      ))
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

    "fail if the path is null" in {
      ( ("foo" -> JNull) : JValue ).optional[Int]("foo") mustEqual Failure(JsonErrors.Malformed("foo", "expected int, found null"))
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

    "fail if the path is null" in {
      ( ("foo" -> JNull) : JValue ).optional[Int]("foo") mustEqual Failure(JsonErrors.Malformed("foo", "expected int, found null"))
    }
  }

  "json.nullable - single argument" should {
    "succeed in normal circumstances" in {
      data.nullable[Int]("num") mustEqual Success(Some(123))
    }

    "succeed with full JPaths" in {
      data.nullable[Int]("obj.a.b.c") mustEqual Success(Some(123))
      data.nullable[Int]("arr[1]") mustEqual Success(Some(2))
    }

    "fail if a type conversion is impossible" in {
      data.nullable[Int]("str") mustEqual Failure(JsonErrors.Malformed("str", "expected int, found \"abc\""))
      data.nullable[Int]("obj.a.b") mustEqual Failure(JsonErrors.Malformed("obj.a.b", "expected int, found {\"c\":123}"))
    }

    "succeed if the path is missing" in {
      data.nullable[Int]("foo") mustEqual Success(Option.empty[Int])
      data.nullable[Int]("obj.a.b.d") mustEqual Success(Option.empty[Int])
    }

    "succeed if the path is null" in {
      ( ("foo" -> JNull) : JValue ).nullable[Int]("foo") mustEqual Success(Option.empty[Int])
    }
  }

  "json.nullable - two arguments" should {
    "succeed in normal circumstances" in {
      data.nullable[Int]("num", -1) mustEqual Success(123)
    }

    "succeed with full JPaths" in {
      data.nullable[Int]("obj.a.b.c", -1) mustEqual Success(123)
      data.nullable[Int]("arr[1]", -1) mustEqual Success(2)
    }

    "fail if a type conversion is impossible" in {
      data.nullable[Int]("str", -1) mustEqual Failure(JsonErrors.Malformed("str", "expected int, found \"abc\""))
      data.nullable[Int]("obj.a.b", -1) mustEqual Failure(JsonErrors.Malformed("obj.a.b", "expected int, found {\"c\":123}"))
    }

    "succeed if the path is missing" in {
      data.nullable[Int]("foo", -1) mustEqual Success(-1)
      data.nullable[Int]("obj.a.b.d", -1) mustEqual Success(-1)
    }

    "succeed if the path is null" in {
      ( ("foo" -> JNull) : JValue ).nullable[Int]("foo", 123) mustEqual Success(123)
    }
  }

  "tuple" should {
    "succeed with 2 arguments" in {
      (data.mandatory[Int]("num") tuple data.mandatory[String]("str")) mustEqual Success((123, "abc"))
      (data.mandatory[Int]("foo") tuple data.mandatory[String]("str")) mustEqual Failure(JsonErrors(JsonError.Missing("foo")))
      (data.mandatory[Int]("num") tuple data.mandatory[String]("bar")) mustEqual Failure(JsonErrors(JsonError.Missing("bar")))
      (data.mandatory[Int]("foo") tuple data.mandatory[String]("bar")) mustEqual Failure(JsonErrors(JsonError.Missing("foo"), JsonError.Missing("bar")))
    }

    "succeed with 3 arguments" in {
      tuple(data.mandatory[Int]("num"), data.mandatory[String]("str"), data.mandatory[Boolean]("bool")) mustEqual Success((123, "abc", true))
      tuple(data.mandatory[Int]("foo"), data.mandatory[String]("bar"), data.mandatory[Boolean]("baz")) mustEqual Failure(
        JsonErrors(
          JsonError.Missing("foo"),
          JsonError.Missing("bar"),
          JsonError.Missing("baz")
        )
      )
    }
  }

  "buildOptionFormat" should {
    "distinguish between JNothing and JNull" in {
      implicit val optFormat = buildOptionFormat[Int]

      val json: JValue =
        ("a" -> JNothing) ~
        ("b" -> JNull) ~
        ("c" -> 123) ~
        ("d" -> "123")

      json.optional[Option[Int]]("a") mustEqual Success(None)
      json.optional[Option[Int]]("b") mustEqual Success(Some(None))
      json.optional[Option[Int]]("c") mustEqual Success(Some(Some(123)))
      json.optional[Option[Int]]("d") mustEqual Failure(JsonErrors(JsonError.Malformed("d", "expected int, found \"123\"")))

      json.optional[Int]("a") mustEqual Success(None)
      json.optional[Int]("b") mustEqual Failure(JsonErrors(JsonError.Malformed("b", "expected int, found null")))
      json.optional[Int]("c") mustEqual Success(Some(123))
      json.optional[Int]("d") mustEqual Failure(JsonErrors(JsonError.Malformed("d", "expected int, found \"123\"")))
    }
  }
}