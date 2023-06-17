package scalka.kernel

import scalka.kernel.types._

trait Monad[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Bind[K, Ob, ->, F] {
  def pure: Transform[IdK[K], F]
}

trait SetMonad[F[_]] extends Monad[Any, Scal, Function, F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def apply[A: Scal]: Scal[F[A]] = summon
  def fmap[A: Scal, B: Scal](f: A => B): F[A] => F[B] =
    fa => flatMap(fa)(f andThen pure.relation[B])

  val flatten: Transform[[A] =>> F[F[A]], F] =
    new Transform[[A] =>> F[F[A]], F] {
      def domain[A: Scal]: Scal[F[F[A]]] = summon
      def relation[A: Scal]: F[F[A]] => F[A] = ffa => flatMap(ffa)(identity)
      def codomain[A: Scal]: Scal[F[A]] = summon
    }

  override def bind[A: Scal, B: Scal](f: A => F[B]): F[A] => F[B] =
    fa => flatMap(fa)(f)
}
