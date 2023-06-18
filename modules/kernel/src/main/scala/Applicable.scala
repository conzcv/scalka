package scalka.kernel

import scalka.kernel.types._

trait Applicable[Ob[_], ->[_, _]] {
  def apply[A: Ob, B: Ob](a: A, f: A -> B): B
}

object Applicable {
  given Applicable[Scal, <:<] = new Applicable[Scal, <:<] {
    def apply[A: Scal, B: Scal](a: A, f: A <:< B): B = f(a)
  }
}