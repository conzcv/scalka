package scalka.kernel

trait Nat[
  SKind <: AnyKind,
  SOb[A <: SKind],

  DKind <: AnyKind,
  DOb[A <: DKind],
  DRel[A <: DKind, B <: DKind],

  F[A <: SKind] <: DKind,
  G[A <: SKind] <: DKind
] {
  type ~>[A <: DKind, B <: DKind] = Morphism[DKind, DOb, DRel, A, B]
  def apply[A <: SKind](ob: SOb[A]): F[A] ~> G[A]
}

object Nat {
  def apply[
    SKind <: AnyKind,
    SOb[A <: SKind],

    DKind <: AnyKind,
    DOb[A <: DKind],
    DRel[A <: DKind, B <: DKind],
    
    F[A <: SKind] <: DKind,
    G[A <: SKind] <: DKind
  ](f: FunctionK[SKind, SOb, [A <: SKind] =>> Morphism[DKind, DOb, DRel, F[A], G[A]]]) =
    new Nat[SKind, SOb, DKind, DOb, DRel, F, G] {
      def apply[A <: SKind](ob: SOb[A]): F[A] ~> G[A] = f(ob)
    }
}
