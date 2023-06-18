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

trait ScalFunctor[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]]
  extends Functor[K, Ob, ->, Any, Scal, Function, F] {
  def map[A <: K: Ob, B <: K: Ob](fa: F[A])(f: A -> B): F[B]
  final def apply[A <: K: Ob]: Scal[F[A]] = summon
  final def fmap[A <: K: Ob, B <: K: Ob](f: A -> B): F[A] => F[B] = map(_)(f)
}

final case class ScalForgetful[Ob[_], ->[_, _]](applicable: Applicable[Ob, ->]) extends ScalFunctor[Any, Ob, ->, Id] {
  def map[A: Ob, B: Ob](fa: A)(f: A -> B): B = applicable(fa, f)
}

final case class ScalKForgetful[K <: AnyKind, Ob[F[A <: K]], ->[F[A <: K], G[B <: K]]](applicableK: ApplicableK[K, Ob, ->])
  extends Functor[AnyK[K], Ob, ->, AnyK[K], ScalKCons[K], FunctionKCons[K], IdK[AnyK[K]]] {
    final def apply[F[A <: K] : Ob]: ScalK[K, F] = summon
    def fmap[F[A <: K]: Ob, G[B <: K]: Ob](f: F -> G): FunctionK[K, F, G] = applicableK.toFunctionK(f)
  }

object Functor extends FunctorInstances