package scalka.kernel

import scalka.kernel.types.Kleisli
import scalka.kernel.types.Scal

trait Traverse[Ob[_], ->[_, _], L[_], F[_]]
  extends Endofunctor[Ob, [A, B] =>> Kleisli[->, F, A, B], L] {
    def traverse[A: Ob, B: Ob](f: A -> F[B]): L[A] -> F[L[B]] = fmap(f)
  }

trait ScalTraverse[L[_], F[_]] extends Traverse[Scal, Function, L, F] {
  final def apply[A: Scal]: Scal[L[A]] = summon

  def traverse[A, B](la: L[A])(f: A => F[B]): F[L[B]]
  override def traverse[A: Scal, B: Scal](f: A => F[B]): L[A] => F[L[B]] =
    la => traverse(la)(f)

  def fmap[A: Scal, B: Scal](f: Kleisli[Function, F, A, B]): Kleisli[Function, F, L[A], L[B]] =
    la => traverse(la)(f)
}