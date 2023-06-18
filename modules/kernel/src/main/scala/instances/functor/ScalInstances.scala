package scalka.instances.functor

import scalka.kernel.ScalEndofunctor
import scalka.kernel.types.Scal

trait ScalInstances {
  given [X]: ScalEndofunctor[(X => _)] = new ScalEndofunctor[(X => _)] {

    override def map[A: Scal, B: Scal](xa: X => A)(f: A => B): X => B = xa andThen f
  }

  given [X]: ScalEndofunctor[(X, _)] = new ScalEndofunctor[(X, _)] {  

    override def map[A: Scal, B: Scal](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))  
  }
}