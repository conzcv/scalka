package scalka.kernel

trait Endofunctor[
  K <: AnyKind,
  Ob[A <: K],
  Rel[A <: K, B <: K],
  F[A <: K] <: K
] extends Fun[K, Ob, Rel, K, Ob, Rel, F] {
  type ~>[A <: K, B <: K] = ->[A, B]
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[F[A]]
  type From[G[A <: K] <: K, A <: K] = Morphism[K, Ob, Rel, G[A], F[A]]
  type To[G[A <: K] <: K, A <: K] = Morphism[K, Ob, Rel, F[A], G[A]]
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, Rel, F, G]
}

