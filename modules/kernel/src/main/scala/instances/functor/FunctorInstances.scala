package scalka.instances.functor

import scalka.kernel._

trait FunctorInstances extends ScalInstances {
  given [Ob[_], ->[_, _]](using A: Applicable[Ob, ->]): ScalForgetful[Ob, ->] =
    ScalForgetful(A)
}
