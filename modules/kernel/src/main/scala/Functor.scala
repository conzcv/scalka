package scalka.kernel

import scalka.instances.functor.FunctorInstances

trait Functor[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D
] {
  def fmap[A <: S: SOb, B <: S: SOb](f: A -> B): F[A] ~> F[B]
  def apply[A <: S: SOb]: DOb[F[A]]
}

object Functor extends FunctorInstances