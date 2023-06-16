package scalka.kernel

import scalka.kernel.types._
import scalka.instances.functor.FunctorInstances

trait Functor[
  SKind <: AnyKind,
  SOb[A <: SKind],
  ->[A <: SKind, B <: SKind],

  DKind <: AnyKind,
  DOb[A <: DKind],
  ~>[A <: DKind, B <: DKind],
  
  F[A <: SKind] <: DKind
] {
  def fmap[A <: SKind: SOb, B <: SKind: SOb](f: A -> B): F[A] ~> F[B]
  def apply[A <: SKind: SOb]: DOb[F[A]]
}

sealed trait HomCovariant[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  R <: K
] extends Functor[K, Ob, ->, Any, Scal, Function, [B <: K] =>> R -> B]

object HomCovariant {
  def apply[
    K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
    R <: K: Ob
  ](using  C: Category[K, Ob, ->]): HomCovariant[K, Ob, ->, R] = new HomCovariant[K, Ob, ->, R] {
    def fmap[A <: K: Ob, B <: K: Ob](f: A -> B): (R -> A) => (R -> B) =
      C.compose(f, _)

    def apply[A <: K: Ob]: Scal[R -> A] =
      summon
  }
}

object Functor extends FunctorInstances