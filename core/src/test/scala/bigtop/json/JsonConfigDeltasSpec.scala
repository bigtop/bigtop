package bigtop.json

import org.specs2.mutable.Specification
import bigtop.json.JsonFormatters._
import bigtop.problem._
import bigtop.util.ValidationMatchers._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

class JsonConfigDeltasSpec extends Specification {

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

  val stringDeltas = JsonConfigDeltas(
    JPath("foo")     -> JString("newfoo"),
    JPath("bar.baz") -> JString("newbaz")
  )

  val nullDeltas = JsonConfigDeltas(
    JPath("foo")     -> JNull,
    JPath("bar.arr") -> JNull
  )

  "JsonConfigDeltas.apply(JsonConfig)" should {
    "set values for the first time" in {
      stringDeltas(JsonConfig.Empty) mustEqual JsonConfig(
        ("foo" -> "newfoo") ~
        ("bar" -> ("baz" -> "newbaz"))
      )
    }

    "overwrite existing values" in {
      stringDeltas(JsonConfig(data)) mustEqual JsonConfig(
        ("foo" -> "newfoo") ~
        ("bar" -> {
          ("str" -> "abc") ~
          ("arr" -> List("a", "b", "c")) ~
          ("baz" -> "newbaz")
        })
      )
    }

    "set values to nothing when nothing or null specified as a value" in {
      nullDeltas(JsonConfig(data)) mustEqual JsonConfig(
        ("foo" -> JNothing) ~
        ("bar" -> {
          ("str" -> "abc") ~
          ("arr" -> JNothing)
        })
      )
    }
  }
}