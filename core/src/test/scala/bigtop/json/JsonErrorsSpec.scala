package bigtop.json

import org.specs2.mutable.Specification
import bigtop.util.ValidationMatchers._
import bigtop.json._
import bigtop.json.JsonFormatters._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import scalaz.Scalaz._

class JsonErrorsSpec extends Specification {

  "JsonError.format.read" should {
    "succeed given valid JSON" in {
      JsonParser.parse("""
        {
          "typename":"missing",
          "path":".username",
          "message":"This value is required."
        }
      """).as[JsonError] mustEqual Success(JsonError.Missing("username"))
    }
  }

  "JsonErrors.format.read" should {
    "succeed given valid JSON" in {
      JsonParser.parse("""
        [{
          "typename":"missing",
          "path":".username",
          "message":"This value is required."
        }]
      """).as[JsonErrors] mustEqual Success(
        JsonErrors(List(
          JsonError.Missing("username")
        ))
      )
    }
  }

}
