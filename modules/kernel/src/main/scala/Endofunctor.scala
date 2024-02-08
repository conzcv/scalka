package scalka.kernel

import scalka.kernel.types.Scal

trait Endofunctor[Ob[_], ->[_, _], F[_]] extends Functor[Ob, ->, Ob, ->, F] {
  val category: Category[Ob, ->]
  type Transform[F[A], G[A]] = Nat[Ob, Ob, ->, F, G]
}

trait ScalEndofunctor[F[_]] extends Endofunctor[Scal, Function, F] with ScalFunctor[Scal, Function, F]