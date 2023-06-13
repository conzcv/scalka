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

trait Endofunctor[
  Kind <: AnyKind,
  Ob[A <: Kind],
  Arr[A <: Kind, B <: Kind],
  F[A <: Kind] <: Kind
] extends Fun[Kind, Ob, Arr, Kind, Ob, Arr, F]

sealed trait FunInstances {
  val liftSubtyping = new Fun[Any, Scal, <:<, Any, Scal, Function, Id] {
    def fmap[A, B](f: A -> B): A ~> B = f.asInstanceOf[A ~> B]
  }
}

object Fun extends FunInstances