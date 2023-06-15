package scalka.kernel

import scalka.syntax.functionK._

trait Nat[
  SKind <: AnyKind,
  SOb[A <: SKind],

  DKind <: AnyKind,
  DOb[A <: DKind],
  DArr[A <: DKind, B <: DKind],

  F[A <: SKind] <: DKind,
  G[A <: SKind] <: DKind
] {
  type ~>[A <: DKind, B <: DKind] = Morphism[DKind, DOb, DArr, A, B]
  def apply[A <: SKind](ob: SOb[A]): F[A] ~> G[A]
}

object Nat {
  def apply[
    SKind <: AnyKind,
    SOb[A <: SKind],

    DKind <: AnyKind,
    DOb[A <: DKind],
    DArr[A <: DKind, B <: DKind],
    
    F[A <: SKind] <: DKind,
    G[A <: SKind] <: DKind
  ](f: FunctionK[SKind, SOb, [A <: SKind] =>> Morphism[DKind, DOb, DArr, F[A], G[A]]]) =
    new Nat[SKind, SOb, DKind, DOb, DArr, F, G] {
      def apply[A <: SKind](ob: SOb[A]): F[A] ~> G[A] = f(ob)
    }
}
