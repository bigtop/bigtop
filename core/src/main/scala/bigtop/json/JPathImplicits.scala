package bigtop.json

import blueeyes.json._

object JPathImplicits extends JPathImplicits

trait JPathImplicits {
  implicit def jPathToJPathW(in: JPath) =
    JPathW(in)
}

case class JPathW(val inner: JPath) {
  def toNormalizedString = {
    val ans = inner.toString
    if(ans startsWith ".") {
      ans substring 1
    } else {
      ans
    }
  }
}