package org.ensime.sexp.util

trait ThreadLocalSupport {
  protected def local[T](t: => T) = new ThreadLocal[T] {
    override def initialValue = t
  }
}
