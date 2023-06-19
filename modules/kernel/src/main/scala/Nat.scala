package scalka.kernel

import scalka.kernel.types.Scal
import scalka.syntax.category.>>>

trait Nat[SOb[_], DOb[_], ~>[_, _], F[_], G[_]] {
  def domain[A: SOb]: DOb[F[A]]
  def relation[A: SOb]: F[A] ~> G[A]
  def codomain[A: SOb]: DOb[G[A]]
}

trait NaturalIso[COb[_], DOb[_], ~>[_, _], F[_], G[_]]
  extends Nat[COb, DOb, ~>, F, G] { self =>
    def reverseRelation[A: COb]: G[A] ~> F[A]

    final def reverse: Nat[COb, DOb, ~>, G, F] = new Nat[COb, DOb, ~>, G, F] {
      def domain[A: COb]: DOb[G[A]] = self.codomain[A]
      def relation[A: COb]: G[A] ~> F[A] = reverseRelation[A]
      def codomain[A: COb]: DOb[F[A]] = self.domain[A]
    }
  }

object Nat {
  def compose[SOb[A], DOb[A], ~>[A, B],F[A], G[A], H[A]](
    f: Nat[SOb, DOb, ~>, G, H],
    g: Nat[SOb, DOb, ~>, F, G]
  )(using C: Category[DOb, ~>]): Nat[SOb, DOb, ~>, F, H] =
    new Nat[SOb, DOb, ~>, F, H] {
      def domain[A: SOb]: DOb[F[A]] = g.domain[A]
      def relation[A: SOb]: F[A] ~> H[A] =
        given DOb[F[A]] = g.domain[A]
        given DOb[G[A]] = f.domain[A]
        given DOb[H[A]] = f.codomain[A]
        g.relation[A] >>> f.relation[A]
      def codomain[A: SOb]: DOb[H[A]] = f.codomain[A]
    }
}

trait ScalNat[Ob[_], F[_], G[_]] extends Nat[Ob, Scal, Function, F, G] {
  def run[A: Ob](fa: F[A]): G[A]
  final def domain[A: Ob]: Scal[F[A]] = summon
  final def relation[A: Ob]: F[A] => G[A] = run[A]
  final def codomain[A: Ob]: Scal[G[A]] = summon
}
