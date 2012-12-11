package bigtop
package util

/*
 * Utility for printing stuff in UTF-8, unlike println
 */
object Println {
  import java.io._

  val out = new PrintStream(System.out, true, "UTF-8");

  def apply(message: Array[Byte]): Unit =
    out.println(new String(message, "UTF-8"));

  def apply(message: String): Unit =
    apply(message.getBytes("UTF-8"))

  def apply(flag: Boolean): Unit =
    if(flag) apply("true") else apply("false")
}
