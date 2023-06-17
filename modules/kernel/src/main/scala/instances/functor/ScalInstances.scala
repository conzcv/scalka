package scalka.instances.functor

import scalka.kernel.types._
import scalka.kernel.{SetEndofunctor, Functor}

trait ScalInstances {
  given [X]: SetEndofunctor[(X => _)] = new SetEndofunctor[(X => _)] {

    override def map[A, B](xa: X => A)(f: A => B): X => B = xa andThen f
  }

  given [X]: SetEndofunctor[(X, _)] = new SetEndofunctor[(X, _)] {  

    override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))  
  }

  given Functor[Any, Scal, <:<, Any, Scal, Function, Id] = new Functor[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A: Scal, B: Scal](f: A <:< B): A => B = f.asInstanceOf[A => B]
    def apply[A: Scal]: Scal[Id[A]] = summon
  }
}