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

final case class SetForgetful[Ob[_], ->[_, _]](applicable: Applicable[Ob, ->]) extends Functor[Any, Ob, ->, Any, Scal, Function, Id] {
  final def apply[A: Ob]: Scal[A] = summon

  def fmap[A: Ob, B: Ob](f: A -> B): A => B = applicable.toFunction(f)
}

final case class ApplicableForgetful[K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]](applicableK: ApplicableK[K, Ob, ->])
  extends Functor[AnyK[K], Ob, ->, AnyK[K], ScalKCons[K], FunctionKCons[K], IdK[AnyK[K]]] {
    final def apply[F[A <: K] : Ob]: ScalK[K, F] = summon
    def fmap[F[A <: K]: Ob, G[B <: K]: Ob](f: F -> G): FunctionK[K, F, G] = applicableK.toFunctionK(f)
  }

object Functor extends FunctorInstances