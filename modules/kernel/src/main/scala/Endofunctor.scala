package scalka.kernel

import scalka.kernel.types.Scal

trait Endofunctor[Ob[A], ->[A, B], F[A]] extends Functor[Ob, ->, Ob, ->, F] {
  val category: Category[Ob, ->]
  type Transform[F[A], G[A]] = Nat[Ob, Ob, ->, F, G]
}

trait ScalEndofunctor[F[_]] extends Endofunctor[Scal, Function, F] with ScalFunctor[Scal, Function, F]