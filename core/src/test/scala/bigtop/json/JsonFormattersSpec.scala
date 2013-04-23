package bigtop.json

import org.specs2.mutable.Specification
import bigtop.json.JsonFormatters._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

import bigtop.json.JsonErrors.Missing.{ apply => missing }
import bigtop.json.JsonErrors.TypeError.{ apply => typeError }

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

    "fail if a type conversion is impossible" in {
      JString("abc").as[Int] mustEqual Failure(typeError("int", "abc"))
    }
  }

  "json.asSeq" should {
    "succeed in normal circumstances" in {
      (List(1, 2, 3) : JValue).asSeq[Int] mustEqual Success(List(1, 2, 3))
    }

    "fail if a type conversion is impossible" in {
      JString("abc").as[Int] mustEqual Failure(typeError("int", "abc"))
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
      data.mandatory[Int]("str") mustEqual Failure(typeError("str", "int", "abc"))
      data.mandatory[Int]("obj.a.b") mustEqual Failure(typeError("obj.a.b", "int", "c" -> 123))
    }

    "fail if the path is missing" in {
      data.mandatory[JValue]("foo") mustEqual Failure(missing("foo"))
      data.mandatory[Int]("obj.a.b.d") mustEqual Failure(missing("obj.a.b.d"))
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
      data.optional[Int]("str") mustEqual Failure(typeError("str", "int", "abc"))
      data.optional[Int]("obj.a.b") mustEqual Failure(typeError("obj.a.b", "int", "c" -> 123))
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
      data.optional[Int]("str", -1) mustEqual Failure(typeError("str", "int", "abc"))
      data.optional[Int]("obj.a.b", -1) mustEqual Failure(typeError("obj.a.b", "int", "c" -> 123))
    }

    "succeed if the path is missing" in {
      data.optional[Int]("foo", -1) mustEqual Success(-1)
      data.optional[Int]("obj.a.b.d", -1) mustEqual Success(-1)
    }
  }

  "tuple" should {
    "succeed with 2 arguments" in {
      (data.mandatory[Int]("num") tuple data.mandatory[String]("str")) mustEqual Success((123, "abc"))
      (data.mandatory[Int]("foo") tuple data.mandatory[String]("str")) mustEqual Failure(missing("foo"))
      (data.mandatory[Int]("num") tuple data.mandatory[String]("bar")) mustEqual Failure(missing("bar"))
      (data.mandatory[Int]("foo") tuple data.mandatory[String]("bar")) mustEqual Failure(missing("foo") ++ missing("bar"))
    }

    "succeed with 3 arguments" in {
      tuple(data.mandatory[Int]("num"), data.mandatory[String]("str"), data.mandatory[Boolean]("bool")) mustEqual Success((123, "abc", true))
      tuple(data.mandatory[Int]("foo"), data.mandatory[String]("bar"), data.mandatory[Boolean]("baz")) mustEqual Failure(missing("foo") ++ missing("bar") ++ missing("baz"))
    }
  }

  "pathFormat" should {
    "read object paths" in {
      val json: JValue = ("a" -> ("b" -> ("c" -> "d")))
      pathFormat("a")(identityFormat).read(json) mustEqual Success(("b" -> ("c" -> "d")) : JValue)
      pathFormat("a.b")(identityFormat).read(json) mustEqual Success(("c" -> "d") : JValue)
      pathFormat("a.b.c")(identityFormat).read(json) mustEqual Success("d" : JValue)
      pathFormat("a.b.c.d")(identityFormat).read(json) mustEqual Failure(missing("a.b.c.d"))
      pathFormat("a.b")(intFormat).read(json) mustEqual Failure(typeError("a.b", "int", "c" -> "d"))
    }

    "write object paths" in {
      pathFormat("a")(intFormat).write(1) mustEqual (("a" -> 1) : JValue)
      pathFormat("a.b")(intFormat).write(1) mustEqual (("a" -> ("b" -> 1)) : JValue)
      pathFormat("a.b.c")(intFormat).write(1) mustEqual (("a" -> ("b" -> ("c" -> 1))) : JValue)
    }

    "read array paths" in {
      val sub0: JValue = "b"
      val sub1: JValue = ("c" -> "d")
      val json: JValue = ("a" -> List(sub0, sub1))
      pathFormat("a")(identityFormat).read(json) mustEqual Success(List(sub0, sub1) : JValue)
      pathFormat("a[0]")(identityFormat).read(json) mustEqual Success(sub0)
      pathFormat("a[1]")(identityFormat).read(json) mustEqual Success(sub1)
      pathFormat("a[1].c")(identityFormat).read(json) mustEqual Success("d" : JValue)
      pathFormat("a[2]")(identityFormat).read(json) mustEqual Failure(missing("a[2]"))
    }

    "write array paths" in {
      pathFormat("a")(intFormat).write(1) mustEqual (("a" -> 1) : JValue)
      pathFormat("a[0]")(intFormat).write(1) mustEqual (("a" -> List(JInt(1))) : JValue)
      pathFormat("a[1]")(intFormat).write(1) mustEqual (("a" -> List(JNothing, JInt(1))) : JValue)
      pathFormat("a[1].b")(intFormat).write(1) mustEqual JObject(List(JField("a", List(JNothing, JObject(List(JField("b", JInt(1))))))))
    }
  }

  "optionFormat" should {
    "distinguish between JNothing and JNull" in {
      val json: JValue =
        ("a" -> JNothing) ~
        ("b" -> JNull) ~
        ("c" -> 123) ~
        ("d" -> "123")

      json.optional[Option[Int]]("a") mustEqual Success(None)
      json.optional[Option[Int]]("b") mustEqual Success(Some(None))
      json.optional[Option[Int]]("c") mustEqual Success(Some(Some(123)))
      json.optional[Option[Int]]("d") mustEqual Failure(typeError("d", "int", "123"))

      json.optional[Int]("a") mustEqual Success(None)
      json.optional[Int]("b") mustEqual Failure(typeError("b", "int", JNull))
      json.optional[Int]("c") mustEqual Success(Some(123))
      json.optional[Int]("d") mustEqual Failure(typeError("d", "int", "123"))
    }
  }

  // Detailed tests are provided for tuple2Format.
  // Other tuple formats are tested for arity only:

  "tuple (2 arguments)" should {
    "read" in {
      val format = tuple(
        (__ \ "a").as[Int],
        (__ \ "b").as[String]
      )

      format.read(("a" -> 123) ~ ("b" -> "abc")) mustEqual Success((123, "abc"))
      format.read(("a" -> "abc") ~ ("b" -> "abc")) mustEqual Failure(typeError("a", "int", "abc"))
      format.read(("a" -> 123) ~ ("b" -> 123)) mustEqual Failure(typeError("b", "string", 123))
      format.read(("a" -> "abc") ~ ("b" -> 123)) mustEqual Failure(typeError("a", "int", "abc") ++ typeError("b", "string", 123))
    }

    "write" in {
      val format = tuple(pathFormat("a")(intFormat), pathFormat("b")(stringFormat))
      format.write((123, "abc")) mustEqual (("a" -> 123) ~ ("b" -> "abc"))
    }
  }

  "tuple (3 arguments)" should {
    type Data = (Int, Int, Int)
    implicit val format = tuple((__ \ "a").as[Int], (__ \ "b").as[Int], (__ \ "c").as[Int])
    val json = ("a" -> 1) ~ ("b" -> 2) ~ ("c" -> 3)
    val data = (1, 2, 3)
    "read" in { json.as[Data] mustEqual Success(data) }
    "write" in { data.toJson mustEqual json }
  }

  "tuple (4 arguments)" should {
    type Data = (Int, Int, Int, Int)
    implicit val format = tuple((__ \ "a").as[Int], (__ \ "b").as[Int], (__ \ "c").as[Int], (__ \ "d").as[Int])
    val json = ("a" -> 1) ~ ("b" -> 2) ~ ("c" -> 3) ~ ("d" -> 4)
    val data = (1, 2, 3, 4)
    "read" in { json.as[Data] mustEqual Success(data) }
    "write" in { data.toJson mustEqual json }
  }

  "tuple (5 arguments)" should {
    type Data = (Int, Int, Int, Int, Int)
    implicit val format = tuple((__ \ "a").as[Int], (__ \ "b").as[Int], (__ \ "c").as[Int], (__ \ "d").as[Int], (__ \ "e").as[Int])
    val json = ("a" -> 1) ~ ("b" -> 2) ~ ("c" -> 3) ~ ("d" -> 4) ~ ("e" -> 5)
    val data = (1, 2, 3, 4, 5)
    "read" in { json.as[Data] mustEqual Success(data) }
    "write" in { data.toJson mustEqual json }
  }

  "tuple (6 arguments)" should {
    type Data = (Int, Int, Int, Int, Int, Int)
    implicit val format = tuple((__ \ "a").as[Int], (__ \ "b").as[Int], (__ \ "c").as[Int], (__ \ "d").as[Int], (__ \ "e").as[Int], (__ \ "f").as[Int])
    val json = ("a" -> 1) ~ ("b" -> 2) ~ ("c" -> 3) ~ ("d" -> 4) ~ ("e" -> 5) ~ ("f" -> 6)
    val data = (1, 2, 3, 4, 5, 6)
    "read" in { json.as[Data] mustEqual Success(data) }
    "write" in { data.toJson mustEqual json }
  }

  "tuple (7 arguments)" should {
    type Data = (Int, Int, Int, Int, Int, Int, Int)
    implicit val format = tuple((__ \ "a").as[Int], (__ \ "b").as[Int], (__ \ "c").as[Int], (__ \ "d").as[Int], (__ \ "e").as[Int], (__ \ "f").as[Int], (__ \ "g").as[Int])
    val json = ("a" -> 1) ~ ("b" -> 2) ~ ("c" -> 3) ~ ("d" -> 4) ~ ("e" -> 5) ~ ("f" -> 6) ~ ("g" -> 7)
    val data = (1, 2, 3, 4, 5, 6, 7)
    "read" in { json.as[Data] mustEqual Success(data) }
    "write" in { data.toJson mustEqual json }
  }
}