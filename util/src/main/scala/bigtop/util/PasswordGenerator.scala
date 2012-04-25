package bigtop
package util

import scala.io.Source
import scala.collection.mutable.StringBuilder
import scala.util.Random

/*
 * Thread-safe password generator. Creates passwords by concatentating phonemes and symbols. The resulting passwords are hard to guess (we hope)
 */
object PasswordGenerator {

  val phonemes = Seq(
    "a", "e", "i", "o", "u",
    "ae", "ee", "ie", "oe", "ue", "oo",
    "ar", "ur", "or", "au", "er",
    "ow", "oi", "air", "ear",
    "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "r", "s", "t", "v", "w",
    "wh", "y", "z", "th", "ch", "sh", "zh", "ng",

    "A", "E", "I", "O", "U",
    "AE", "EE", "IE", "OE", "UE", "OO",
    "AR", "UR", "OR", "AU", "ER",
    "OW", "OI", "AIR", "EAR",
    "B", "D", "F", "G", "H", "J", "K", "L", "M", "N", "P", "R", "S", "T", "V", "W",
    "WH", "Y", "Z", "TH", "CH", "SH", "ZH", "NG"
  )

  val symbols = Seq(
    "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "?", "+", "-"
  )

  val minPasswordLength = 8
  val rng = new Random()

  def sample(choices: Seq[String]): String = {
    synchronized {
      choices(rng.nextInt(choices.length))
    }
  }

  def make(length: Int = minPasswordLength) : String = {
    val password = new StringBuilder(length + 4)
    while(password.length < length) {
      password append sample(phonemes)
      password append sample(phonemes)
      password append sample(symbols)
    }

    password.toString
  }

}
