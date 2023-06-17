package scalka.kernel

import scalka.kernel.types._

trait Applicable[Ob[_], ->[_, _]] {
  def toFunction[A: Ob, B: Ob](f: A -> B): A => B
}

object Applicable {
  given Applicable[Scal, <:<] = new Applicable[Scal, <:<] {
    def toFunction[A: Scal, B: Scal](f: A <:< B): A => B = f
  }
}