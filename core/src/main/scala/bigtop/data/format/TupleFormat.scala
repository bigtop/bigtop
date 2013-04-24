package bigtop.data
package format

import bigtop.concurrent._
import bigtop.problem._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

trait TupleFormats {
  def tupleReader[R, A, B](a : DataReader[A, R], b : DataReader[B, R]): DataReader[(A, B), R] =
    new DataReader[(A, B), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in)).tupled
    }

  def tupleReader[R, A, B, C](a: DataReader[A, R], b: DataReader[B, R], c: DataReader[C, R]): DataReader[(A, B, C), R] =
    new DataReader[(A, B, C), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in) |@| c.read(in)).tupled
    }

  def tupleReader[R, A, B, C, D](a: DataReader[A, R], b: DataReader[B, R], c: DataReader[C, R], d: DataReader[D, R]): DataReader[(A, B, C, D), R] =
    new DataReader[(A, B, C, D), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in)).tupled
    }

  def tupleReader[R, A, B, C, D, E](a: DataReader[A, R], b: DataReader[B, R], c: DataReader[C, R], d: DataReader[D, R], e: DataReader[E, R]): DataReader[(A, B, C, D, E), R] =
    new DataReader[(A, B, C, D, E), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in)).tupled
    }

  def tupleReader[R, A, B, C, D, E, F](a: DataReader[A, R], b: DataReader[B, R], c: DataReader[C, R], d: DataReader[D, R], e: DataReader[E, R], f: DataReader[F, R]): DataReader[(A, B, C, D, E, F), R] =
    new DataReader[(A, B, C, D, E, F), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in) |@| f.read(in)).tupled
    }

  def tupleReader[R, A, B, C, D, E, F, G](a: DataReader[A, R], b: DataReader[B, R], c: DataReader[C, R], d: DataReader[D, R], e: DataReader[E, R], f: DataReader[F, R], g: DataReader[G, R]): DataReader[(A, B, C, D, E, F, G), R] =
    new DataReader[(A, B, C, D, E, F, G), R] {
      def read(in: R) =
        (a.read(in) |@| b.read(in) |@| c.read(in) |@| d.read(in) |@| e.read(in) |@| f.read(in) |@| g.read(in)).tupled
    }
}
