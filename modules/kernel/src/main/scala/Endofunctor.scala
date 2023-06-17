package scalka.kernel

import scalka.kernel.types.Scal

trait Endofunctor[
  K <: AnyKind,
  Ob[A <: K],
  ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Functor[K, Ob, ->, K, Ob, ->, F] {
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[G[A]]
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, ->, F, G]
}

trait SetEndofunctor[F[_]] extends Endofunctor[Any, Scal, Function, F] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def apply[A: Scal]: Scal[F[A]] = summon

  def fmap[A: Scal, B: Scal](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)
}