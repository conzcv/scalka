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

sealed trait HomCovariant[
  K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K],
  R <: K
] extends Functor[K, Ob, Rel, Any, Scal, Function, [B <: K] =>> Morphism[K, Ob, Rel, R, B]]

object HomCovariant {
  def apply[
    K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K],
    R <: K
  ](using  C: Category[K, Ob, Rel]): HomCovariant[K, Ob, Rel, R] = new HomCovariant[K, Ob, Rel, R] {
    def fmap[A <: K, B <: K](f: A -> B): (R -> A) ~> (R -> B) =
      val relation: (R -> A) => (R -> B) = _ >> f
      Morphism.fromRelation(relation)
  }
}

sealed trait FunInstances {
  val liftSubtyping = new Functor[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A, B](f: A -> B): A ~> B = f.asInstanceOf[A ~> B]
  }
}

object Functor extends FunInstances