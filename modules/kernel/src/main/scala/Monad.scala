package scalka.kernel

import scalka.kernel.types._


trait Monad[Ob[_], ->[_, _], F[_]] extends Pure[Ob, ->, F] with Flatten[Ob, ->, F] {
  override def apply[A: Ob]: Ob[F[A]] = pure.codomain[A]
}

object Monad {
  given [Ob[_], ->[_, _], F[_]](using F: Free[Ob, ->, F]): Monad[Scal, Function, F] =
    fromAdjunction(F)

  given [Ob[_], ->[_, _], F[_]](using C: Cofree[Ob, ->, F]): Monad[Ob, ->, F] =
    fromAdjunction(C)

  def fromAdjunction[SOb[_], ->[_, _], DOb[_], ~>[_, _], R[_], L[_]](a: Adjunction[SOb, ->, DOb, ~>, R, L]): Monad[DOb, ~>, R o L] =
    new Monad[DOb, ~>, R o L]  { self =>
      val category = a.rightCategory
      val pure: Transform[Id, R o L] = a.unit

      val flatten: Transform[R o L o R o L, R o L] =
        new Transform[R o L o R o L, R o L] {
          def domain[A: DOb]: DOb[(R o L o R o L)[A]] =
            given rla: DOb[(R o L)[A]] = self[A]
            self[(R o L)[A]]

          def relation[A: DOb]: (R o L o R o L)[A] ~> (R o L)[A] =
            given rla: DOb[(R o L)[A]] = self[A]
            given la: SOb[L[A]] = a.left[A]
            given lrla: SOb[(L o R o L)[A]] = a.left[R[L[A]]]
            a.right.fmap(a.rightAdjunct(fmap(a.rightCategory.id[A])))

          def codomain[A: DOb]: DOb[(R o L)[A]] = self[A]
        }

      def fmap[A: DOb, B: DOb](f: A ~> B): (R o L)[A] ~> (R o L)[B] =
        given la: SOb[L[A]] = a.left[A]
        given lb: SOb[L[B]] = a.left[B]
        a.right.fmap(a.left.fmap(f))
    }
}

trait ScalMonad[F[_]] extends Monad[Scal, Function, F] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def fmap[A: Scal, B: Scal](f: A => B): F[A] => F[B] =
    fa => flatMap(fa)(pure[B] compose f)

  val pure: Transform[Id, F] =
    new Transform[Id, F] {
      def domain[A: Scal]: Scal[A] = summon
      def relation[A: Scal]: A => F[A] = pure[A]
      def codomain[A: Scal]: Scal[F[A]] = summon
    }

  val flatten: Transform[F o F, F] =
    new Transform[F o F, F] {
      def domain[A: Scal]: Scal[F[F[A]]] = summon
      def relation[A: Scal]: F[F[A]] => F[A] = ffa => flatMap(ffa)(identity)
      def codomain[A: Scal]: Scal[F[A]] = summon
    }

  override def bind[A: Scal, B: Scal](f: A => F[B]): F[A] => F[B] =
    fa => flatMap(fa)(f)
}
