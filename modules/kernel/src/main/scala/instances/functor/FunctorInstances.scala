package scalka.instances.functor

import scalka.kernel._
import scalka.kernel.types._

trait FunctorInstances extends ScalInstances {

  given [K <: AnyKind, A <: K]: Functor[AnyK[K], ScalKCons[K], FunctionKCons[K], Any, Scal, Function, [F[B <: K]] =>> F[A]] =
    new Functor[AnyK[K], ScalKCons[K], FunctionKCons[K], Any, Scal, Function, [F[B <: K]] =>> F[A]] {
      def fmap[F[B <: K]: ScalKCons[K], G[B <: K]: ScalKCons[K]](f: FunctionK[K, F, G]): F[A] => G[A] =
        fa => f.apply(fa)
      def apply[F[B <: K]: ScalKCons[K]]: Scal[F[A]] = summon
    }

  given [Ob[_], ->[_, _]](using A: Applicable[Ob, ->]): ScalForgetful[Ob, ->] =
    ScalForgetful(A)

  given [K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]](using A: ApplicableK[K, Ob, ->]): ScalKForgetful[K, Ob, ->] =
    ScalKForgetful(A)
}
