package scalka.kernel

trait Endofunctor[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Fun[K, Ob, Arr, K, Ob, Arr, F] {
  final type ~>[A <: K, B <: K] = ->[A, B]
  final type Transform[F[A <: K] <: K, G[A <: K] <: K] = Nat[K, Ob, K, Ob, Arr, F, G]
}

