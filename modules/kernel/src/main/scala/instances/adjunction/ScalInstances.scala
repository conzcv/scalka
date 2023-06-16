package scalka.instances.adjunction

import scalka.kernel.types._
import scalka.kernel._

trait ScalInstances {
  given [X]: ScalAdjunction1K[X => _, (X, _)] =
    new ScalAdjunction1K[X => _, (X, _)] {
      def left[A: Scal, B: Scal](f: ((X, A)) => B): A => (X => B) =
        a => f(_, a)
        
      def right[A: Scal, B: Scal](f: A => (X => B)): ((X, A)) => B =
        (x, a) => f(a)(x)

      val S = summon
      val D = summon
      val L = summon
      val R = summon
    }
}
