package scalka.kernel

import scalka.kernel.types._
import scalka.instances.functor.FunctorInstances
import scalka.syntax.category.* 

trait Functor[
  S <: AnyKind,
  SOb[A <: S],
  ->[A <: S, B <: S],

  D <: AnyKind,
  DOb[A <: D],
  ~>[A <: D, B <: D],
  
  F[A <: S] <: D
] {
  def fmap[A <: S: SOb, B <: S: SOb](f: A -> B): F[A] ~> F[B]
  def apply[A <: S: SOb]: DOb[F[A]]
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
      ra => ra >>> f
      
    def apply[A <: K: Ob]: Scal[R -> A] =
      summon
  }
}

object Functor extends FunctorInstances