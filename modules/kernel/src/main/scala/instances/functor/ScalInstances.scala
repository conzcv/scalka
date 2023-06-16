package scalka.instances.functor

import scalka.kernel.types._
import scalka.kernel.Functor

trait ScalInstances {
  given [X]: ScalEndofunctor1K[(X => _)] = new ScalEndofunctor1K[(X => _)] {
    def apply[A: Scal]: Scal[X => A] = summon

    def fmap[A: Scal, B: Scal](f: A => B): (X => A) => (X => B) =
      xa => xa andThen f
  }

  given [X]: ScalEndofunctor1K[(X, _)] = new ScalEndofunctor1K[(X, _)] {
    def apply[A: Scal]: Scal[(X, A)] = summon
    
    def fmap[A: Scal, B: Scal](f: A => B): ((X, A)) => (X, B) =
      (x, a) => (x, f(a))
  }

  given Functor[Any, Scal, <:<, Any, Scal, Function, Id] = new Functor[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A: Scal, B: Scal](f: A <:< B): A => B = f.asInstanceOf[A => B]
    def apply[A: Scal]: Scal[Id[A]] = summon
  }
}