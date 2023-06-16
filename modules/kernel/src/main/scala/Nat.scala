package scalka.kernel

trait Nat[
  S <: AnyKind,
  SOb[A <: S],

  D <: AnyKind,
  DOb[A <: D],
  ~>[A <: D, B <: D],

  F[A <: S] <: D,
  G[A <: S] <: D
] {
  def domain[A <: S: SOb]: DOb[F[A]]
  def apply[A <: S: SOb]: F[A] ~> G[A]
  def codomain[A <: S: SOb]: DOb[G[A]]
}
