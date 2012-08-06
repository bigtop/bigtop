package bigtop.problem

import scala.collection.JavaConversions._

case class SourceLocation(val filename: String, val lineNumber: Int) {
  override def toString = {
    "%s:%s".format(filename, lineNumber)
  }
}

/**
 * Idea for this code stolen from Scalatest:
 * http://code.google.com/p/scalatest/source/search?q=StackDepth&origq=StackDepth&btnG=Search+Trunk
 */
object SourceLocation {
  val Unknown = SourceLocation("Unknown.scala", 0)

  private def stackTrace(exn: Throwable): List[StackTraceElement] = {
    exn.getStackTrace.toList
  }

  private def stackTraceElement(stack: List[StackTraceElement], depth: Int): Option[StackTraceElement] = {
    if(stack.length > depth) {
      Some(stack(depth))
    } else {
      None
    }
  }

  private def filename(elem: StackTraceElement): Option[String] = {
    Option(elem.getFileName)
  }

  private def lineNumber(elem: StackTraceElement): Option[Int] = {
    Option(elem.getLineNumber)
  }

  def atDepth(depth: Int): SourceLocation = {
    val stack = stackTrace(new Throwable())
    (
      for {
        elem       <- stackTraceElement(stack, depth + 1)
        filename   <- filename(elem)
        lineNumber <- lineNumber(elem)
      } yield SourceLocation(filename, lineNumber)
    ).getOrElse(Unknown)
  }
}