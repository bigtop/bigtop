package bigtop.json

import org.specs2.mutable.Specification
import bigtop.json.JsonFormatters._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

class JsonConfigSpec extends Specification {

  val data = {
    ("foo" -> {
      ("num" -> 123) ~
      ("arr" -> List(1, 2, 3))
    }) ~
    ("bar" -> {
      ("str" -> "abc") ~
      ("arr" -> List("a", "b", "c"))
    })
  }

  val config = JsonConfig(data)

  "constructor" should {
    "create a config" in {
      config.data mustEqual data
    }

    "create an empty config if no arguments specified" in {
      JsonConfig().data mustEqual JObject.empty
    }
  }

  "apply" should {
    "return a value" in {
      config[Int]("foo.num") mustEqual 123
      config[Int]("foo.arr[1]") mustEqual 2
      config[Seq[Int]]("foo.arr") mustEqual Seq(1, 2, 3)
    }

    "fail if no value is found" in {
      config[Int]("foo.noexist") must throwA[Problem]
    }

    "fail if the value is of the wrong type" in {
      config[String]("foo.num") must throwA[Problem]
    }
  }

  "get" should {
    "return a value" in {
      config.get[Int]("foo.num") must beLike({ case Success(123) => ok })
      config.get[Int]("foo.arr[1]") must beLike({ case Success(2) => ok })
      config.get[Seq[Int]]("foo.arr") must beLike({ case Success(Seq(1, 2, 3)) => ok })
    }

    "fail if no value is found" in {
      config.get[Int]("foo.noexist") must beLike({
        case Failure(Problems.Missing("foo.noexist")) => ok
      })
    }

    "fail if the value is of the wrong type" in {
      config.get[String]("foo.num") must beLike({
        case Failure(Problems.Malformed("foo.num", "expected String, found 123")) => ok
      })
    }
  }

  "set" should {
    "overwrite single values and whole sub-objects" in {
      config.set("foo", "x").set("bar.arr", "y") mustEqual JsonConfig(
        ("foo" -> "x") ~
        ("bar" -> {
          ("str" -> "abc") ~
          ("arr" -> "y")
        })
      )
    }
  }

  "remove" should {
    "remove single values and whole sub-objects" in {
      config.remove("foo").remove("bar.arr") mustEqual JsonConfig(
        ("bar" -> ("str" -> "abc"))
      )
    }
  }
}