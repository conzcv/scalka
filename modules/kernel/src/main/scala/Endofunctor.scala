package scalka.kernel

trait Endofunctor[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Fun[K, Ob, Arr, K, Ob, Arr, F] {
  type ~>[A <: K, B <: K] = ->[A, B]
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[F[A]]
  type From[G[A <: K] <: K, A <: K] = Morphism[K, Ob, Arr, G[A], F[A]]
  type To[G[A <: K] <: K, A <: K] = Morphism[K, Ob, Arr, F[A], G[A]]
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, Arr, F, G]
}

