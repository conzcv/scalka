package scalka.instances.functor

import scalka.kernel.SetForgetful
import scalka.kernel.Applicable
import scalka.kernel.FunctionK
import scalka.kernel.ApplicableK
import scalka.kernel.ApplicableForgetful

trait FunctorInstances extends ScalInstances {

  given [Ob[_], ->[_, _]](using A: Applicable[Ob, ->]): SetForgetful[Ob, ->] =
    new SetForgetful[Ob, ->] {
      def fmap[A: Ob, B: Ob](f: A -> B): A => B = A.toFunction(f)
    }

  given [K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]](using A: ApplicableK[K, Ob, ->]): ApplicableForgetful[K, Ob, ->] =
    new ApplicableForgetful[K, Ob, ->] {
      def fmap[F[A <: K]: Ob, G[B <: K]: Ob](f: F -> G): FunctionK[K, F, G] =
        A.toFunctionK(f)
    }
}
