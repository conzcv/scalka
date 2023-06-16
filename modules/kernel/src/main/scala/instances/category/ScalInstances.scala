package scalka.instances.category

import scalka.kernel.types._
import scalka.kernel.Category

trait ScalInstances {
  given Category[Any, Scal, Function] = new Category[Any, Scal, Function] {
    def compose[A: Scal, B: Scal, C: Scal](f: B => C, g: A => B): A => C =
      f compose g

    def id[A: Scal]: A => A =
      identity
  }

  given Category[Any, Scal, <:<] = new Category[Any, Scal, <:<] {
    def compose[A: Scal, B: Scal, C: Scal](f: B <:< C, g: A <:< B): A <:< C =
      f compose g

    def id[A: Scal]: A <:< A =
      summon
  }
}
