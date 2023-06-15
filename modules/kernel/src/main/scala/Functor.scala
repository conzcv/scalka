package scalka.kernel

import scalka.kernel.types._

trait Functor[
  SKind <: AnyKind,
  SOb[A <: SKind],
  SRel[A <: SKind, B <: SKind],

  DKind <: AnyKind,
  DOb[A <: DKind],
  DRel[A <: DKind, B <: DKind],
  
  F[A <: SKind] <: DKind
] {
  type ->[A <: SKind, B <: SKind] = Morphism[SKind, SOb, SRel, A, B]
  type ~>[A <: DKind, B <: DKind] = Morphism[DKind, DOb, DRel, A, B]

  def fmap[A <: SKind, B <: SKind](f: A -> B): F[A] ~> F[B]
}

sealed trait HomFunctor[
  K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K],
  R <: K
] extends Functor[K, Ob, Rel, Any, Scal, Function, [B <: K] =>> Morphism[K, Ob, Rel, R, B]]

object HomFunctor {
  def apply[
    K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K],
    R <: K
  ](using  C: Category[K, Ob, Rel]): HomFunctor[K, Ob, Rel, R] = new HomFunctor[K, Ob, Rel, R] {
    def fmap[A <: K, B <: K](f: A -> B): (R -> A) ~> (R -> B) =
      val arrow: (R -> A) => (R -> B) = _ >> f
      Morphism.fromArrow(arrow)
  }
}

sealed trait FunInstances {
  val liftSubtyping = new Functor[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A, B](f: A -> B): A ~> B = f.asInstanceOf[A ~> B]
  }
}

object Functor extends FunInstances