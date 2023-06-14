package scalka.kernel

import scalka.kernel.types._

trait Fun[
  SKind <: AnyKind,
  SOb[A <: SKind],
  SArr[A <: SKind, B <: SKind],

  DKind <: AnyKind,
  DOb[A <: DKind],
  DArr[A <: DKind, B <: DKind],
  
  F[A <: SKind] <: DKind
] {
  type ->[A <: SKind, B <: SKind] = Morphism[SKind, SOb, SArr, A, B]
  type ~>[A <: DKind, B <: DKind] = Morphism[DKind, DOb, DArr, A, B]

  def fmap[A <: SKind, B <: SKind](f: A -> B): F[A] ~> F[B]
}

sealed trait HomFunctor[
  K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K],
  R <: K
] extends Fun[K, Ob, Arr, Any, Scal, Function, [B <: K] =>> Morphism[K, Ob, Arr, R, B]]

object HomFunctor {
  def apply[
    K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K],
    R <: K
  ](using  C: Cat[K, Ob, Arr]): HomFunctor[K, Ob, Arr, R] = new HomFunctor[K, Ob, Arr, R] {
    def fmap[A <: K, B <: K](f: A -> B): (R -> A) ~> (R -> B) =
      val arrow: (R -> A) => (R -> B) = _ >> f
      Morphism.fromArrow(arrow)
  }
}

sealed trait FunInstances {
  val liftSubtyping = new Fun[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A, B](f: A -> B): A ~> B = f.asInstanceOf[A ~> B]
  }
}

object Fun extends FunInstances