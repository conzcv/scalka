package scalka.kernel

import scalka.instances.functor.FunctorInstances
import scalka.kernel.types.*

trait Functor[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D
] {
  def fmap[A <: S: SOb, B <: S: SOb](f: A -> B): F[A] ~> F[B]
  def apply[A <: S: SOb]: DOb[F[A]]
}

trait SetForgetful[Ob[_], ->[_, _]] extends Functor[Any, Ob, ->, Any, Scal, Function, Id] {
  final def apply[A: Ob]: Scal[A] = summon
}

trait ApplicableForgetful[K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]]
  extends Functor[AnyK[K], Ob, ->, AnyK[K], ScalKCons[K], FunctionKCons[K], IdK[AnyK[K]]] {
    final def apply[F[A <: K] : Ob]: ScalK[K, F] = summon
  }

object Functor extends FunctorInstances