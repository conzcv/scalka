package scalka.kernel

trait Endofunctor[
  K <: AnyKind,
  Ob[A <: K],
  ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Functor[K, Ob, ->, K, Ob, ->, F] {
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[F[A]]
  type From[G[A <: K] <: K, A <: K] = G[A] -> F[A]
  type To[G[A <: K] <: K, A <: K] = F[A] -> G[A]
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, ->, F, G]
}