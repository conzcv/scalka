package scalka.kernel

import scalka.kernel.types.Scal

trait Endofunctor[Ob[A], ->[A, B], F[A]] extends Functor[Ob, ->, Ob, ->, F] {
  type o[F[A], G[A]] = [A] =>> F[G[A]]
  type Transform[F[A], G[A]] = Nat[Ob, Ob, ->, F, G]
}

trait ScalEndofunctor[F[_]] extends Endofunctor[Scal, Function, F] with ScalFunctor[Scal, Function, F]