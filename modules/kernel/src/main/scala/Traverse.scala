package scalka.kernel

import scalka.kernel.types.Kleisli
import scalka.kernel.types.Scal

trait Traverse[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], L[A <: K] <: K, F[A <: K] <: K]
  extends Endofunctor[K, Ob, [A <: K, B <: K] =>> Kleisli[K, ->, F, A, B], L] {
    def traverse[A <: K: Ob, B <: K: Ob](f: A -> F[B]): L[A] -> F[L[B]] = fmap(f)
  }

trait SetTraverse[L[_], F[_]] extends Traverse[Any, Scal, Function, L, F] {
  final def apply[A: Scal]: Scal[L[A]] = summon

  def traverse[A, B](la: L[A])(f: A => F[B]): F[L[B]]
  override def traverse[A: Scal, B: Scal](f: A => F[B]): L[A] => F[L[B]] =
    la => traverse(la)(f)

  def fmap[A: Scal, B: Scal](f: Kleisli[Any, Function, F, A, B]): Kleisli[Any, Function, F, L[A], L[B]] =
    la => traverse(la)(f)
}