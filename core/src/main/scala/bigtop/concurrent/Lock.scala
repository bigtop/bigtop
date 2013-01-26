package bigtop
package concurrent

import java.util.concurrent.locks.{Lock => JLock, ReentrantLock}

trait Lock {

  /** The lock 'mechanism' is the base implementation of the Lock that does the actual mutual exclusion. */
  val mechanism: JLock

  def locked[A](f: => A): A = {
    this.lock()
    try {
      f
    } finally {
      this.unlock()
    }
  }

  def lock()   = mechanism.lock()
  def unlock() = mechanism.unlock()

}

object Lock {

  def apply(lockImplementation: JLock = new ReentrantLock()): Lock = {
    new Lock {
      val mechanism = lockImplementation
    }
  }

}
