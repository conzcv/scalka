package scalka.instances.adjunction

import scalka.kernel.SetAdjunction

trait ScalInstances {
  given [X]: SetAdjunction[X => _, (X, _)] =
    new SetAdjunction[X => _, (X, _)] {

      def leftAdj[A, B](a: A)(f: ((X, A)) => B): X => B = f(_, a)

      def rightAdj[A, B](xa: (X, A))(f: A => X => B): B = f(xa._2)(xa._1)
      val left = summon
      val right = summon
    }
}
