package scalka.kernel

import scalka.kernel.types.Scal

trait Nat[
  S <: AnyKind, SOb[A <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D, G[A <: S] <: D
] {
  def domain[A <: S: SOb]: DOb[F[A]]
  def relation[A <: S: SOb]: F[A] ~> G[A]
  def codomain[A <: S: SOb]: DOb[G[A]]
}

trait ScalNat[K <: AnyKind, Ob[A <: K], F[A <: K], G[A <: K]]
  extends Nat[K, Ob, Any, Scal, Function, F, G] {
    def run[A <: K: Ob](fa: F[A]): G[A]
    final def domain[A <: K: Ob]: Scal[F[A]] = summon
    final def relation[A <: K: Ob]: F[A] => G[A] = run[A]
    final def codomain[A <: K: Ob]: Scal[G[A]] = summon
  }
