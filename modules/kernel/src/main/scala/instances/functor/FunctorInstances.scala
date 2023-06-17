package scalka.instances.functor

import scalka.kernel.SetForgetful
import scalka.kernel.Applicable
import scalka.kernel.ApplicableK
import scalka.kernel.ApplicableForgetful

trait FunctorInstances extends ScalInstances {

  given [Ob[_], ->[_, _]](using A: Applicable[Ob, ->]): SetForgetful[Ob, ->] =
    SetForgetful(A)

  given [K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]](using A: ApplicableK[K, Ob, ->]): ApplicableForgetful[K, Ob, ->] =
    ApplicableForgetful(A)
}
