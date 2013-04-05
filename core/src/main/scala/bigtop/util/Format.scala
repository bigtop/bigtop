package bigtop
package util

import scalaz.Validation

trait Reader[E, I, O] {
  def read(j: O): Validation[E, I]
}

trait Writer[I, O] {
  def write(t: I): O
}

trait Format[E, I, O] extends Reader[E, I, O] with Writer[I, O]

trait Updater[E, I, O] {
  def update(t: I, j: O): Validation[E, I]
}
