package scalka.kernel

import scalka.kernel.types.Scal
import scalka.syntax.category.>>>

trait Nat[
  S <: AnyKind, SOb[A <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D, G[A <: S] <: D
] {
  def domain[A <: S: SOb]: DOb[F[A]]
  def relation[A <: S: SOb]: F[A] ~> G[A]
  def codomain[A <: S: SOb]: DOb[G[A]]
}

object Nat {
  def compose[
    S <: AnyKind, SOb[A <: S],
    D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
    F[A <: S] <: D, G[A <: S] <: D, H[A <: S] <: D
  ](f: Nat[S, SOb, D, DOb, ~>, G, H], g: Nat[S, SOb, D, DOb, ~>, F, G])(using C: Category[D, DOb, ~>]): Nat[S, SOb, D, DOb, ~>, F, H] =
    new Nat[S, SOb, D, DOb, ~>, F, H] {
      def domain[A <: S: SOb]: DOb[F[A]] = g.domain[A]
      def relation[A <: S: SOb]: F[A] ~> H[A] =
        given DOb[F[A]] = g.domain[A]
        given DOb[G[A]] = f.domain[A]
        given DOb[H[A]] = f.codomain[A]
        g.relation[A] >>> f.relation[A]
      def codomain[A <: S: SOb]: DOb[H[A]] = f.codomain[A]
    }
}

trait ScalNat[K <: AnyKind, Ob[A <: K], F[A <: K], G[A <: K]]
  extends Nat[K, Ob, Any, Scal, Function, F, G] {
    def run[A <: K: Ob](fa: F[A]): G[A]
    final def domain[A <: K: Ob]: Scal[F[A]] = summon
    final def relation[A <: K: Ob]: F[A] => G[A] = run[A]
    final def codomain[A <: K: Ob]: Scal[G[A]] = summon
  }
