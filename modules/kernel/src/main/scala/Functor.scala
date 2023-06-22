package scalka.kernel

import scalka.instances.functor.FunctorInstances
import scalka.kernel.types.*

trait Functor[SOb[_], ->[_, _], DOb[_], ~>[_, _], F[_]] {
  def fmap[A: SOb, B: SOb](f: A -> B): F[A] ~> F[B]
  def apply[A: SOb]: DOb[F[A]]
}

trait ScalFunctor[Ob[A], ->[A, B], F[A]] extends Functor[Ob, ->, Scal, Function, F] {
  def map[A: Ob, B: Ob](fa: F[A])(f: A -> B): F[B]
  def apply[A: Ob]: Scal[F[A]] = summon
  def fmap[A: Ob, B: Ob](f: A -> B): F[A] => F[B] = map(_)(f)
}

final case class ScalForgetful[Ob[_], ->[_, _]](applicable: Applicable[Ob, ->]) extends ScalFunctor[Ob, ->, Id] {
  def map[A: Ob, B: Ob](fa: A)(f: A -> B): B = applicable(fa, f)
}

object Functor extends FunctorInstances