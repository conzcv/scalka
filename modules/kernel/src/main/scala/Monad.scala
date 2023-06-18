package scalka.kernel

import scalka.kernel.types._

trait Monad[Ob[_], ->[_, _], F[_]] extends Bind[Ob, ->, F] {
  def pure: Transform[Id, F]
}

trait ScalMonad[F[_]] extends Monad[Scal, Function, F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
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
