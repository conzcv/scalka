package scalka.kernel

import scalka.kernel.types.Scal

trait Endofunctor[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Functor[K, Ob, ->, K, Ob, ->, F] {
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[G[A]]
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, ->, F, G]
}

trait ScalEndofunctor[F[_]] extends Endofunctor[Any, Scal, Function, F] with ScalFunctor[Any, Scal, Function, F]