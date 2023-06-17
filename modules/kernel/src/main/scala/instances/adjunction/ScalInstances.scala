package scalka.instances.adjunction

import scalka.kernel.types._
import scalka.kernel._

trait ScalInstances {
  given [X]: ScalAdjunction1K[X => _, (X, _)] =
    new ScalAdjunction1K[X => _, (X, _)] {
      def leftAdjunct[A: Scal, B: Scal](f: ((X, A)) => B): A => (X => B) =
        a => f(_, a)
        
      def rightAdjunct[A: Scal, B: Scal](f: A => (X => B)): ((X, A)) => B =
        (x, a) => f(a)(x)

      val leftCategory = summon
      val rightCategory = summon
      val left = summon
      val right = summon
    }
}
