package scalka.instances.functor

import scalka.kernel.SetEndofunctor

trait ScalInstances {
  given [X]: SetEndofunctor[(X => _)] = new SetEndofunctor[(X => _)] {

    override def map[A, B](xa: X => A)(f: A => B): X => B = xa andThen f
  }

  given [X]: SetEndofunctor[(X, _)] = new SetEndofunctor[(X, _)] {  

    override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))  
  }
}