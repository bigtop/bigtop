package bigtop
package util

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.{Gen, Arbitrary, Choose, Prop}
import org.scalacheck.Prop._

class PasswordGeneratorSpec extends Specification with ScalaCheck {

  "PasswordGenerator" should {

    val smallPositiveInt = Gen.choose(1, 256)

    "generate password of correct length" in {
      forAll(smallPositiveInt) {
        (length: Int) => PasswordGenerator.make(length).length >= length
      }
    }

    "generate distinct passwords" in {
      forAll(smallPositiveInt) {
        (length: Int) =>
          PasswordGenerator.make(length) mustNotEqual PasswordGenerator.make(length)
      }
    }

  }

}
