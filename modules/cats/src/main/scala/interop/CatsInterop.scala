package scalka.interop.cats

import cats.Applicative
import cats.arrow.{Category => CatsCategory}
import cats.{Functor => CatsFunctor}
import cats.{Monad => CatsMonad}
import cats.{Traverse => CatsTraverse}
import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.category.* 

given [->[_, _]: CatsCategory]: Category[Scal, ->] = new Category[Scal, ->]  {

  def compose[A: Scal, B: Scal, C: Scal](f: B -> C, g: A -> B): A -> C =
    g >>> f

  def id[A: Scal]: A -> A =
    CatsCategory[->].id[A]
}

given [F[_]: CatsFunctor]: ScalEndofunctor[F] =
  new ScalEndofunctor[F] {
    def map[A: Scal, B: Scal](fa: F[A])(f: A => B): F[B] =
      CatsFunctor[F].map(fa)(f)
  }

given [F[_]: CatsMonad]: ScalMonad[F] =
  new ScalMonad[F]{
    val category: Category[Scal, Function] =
      summon

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      CatsMonad[F].flatMap(fa)(f)

    val pure: Transform[Id, F] =
      new Transform[Id, F] {
        def domain[A: Scal]: Scal[A] = summon
        def relation[A: Scal]: A => F[A] = CatsMonad[F].pure
        def codomain[A: Scal]: Scal[F[A]] = summon
      }
  }

given [L[_]: CatsTraverse, G[_]: Applicative]: ScalTraverse[L, G] =
  new ScalTraverse[L, G] {
    def traverse[A, B](la: L[A])(f: A => G[B]): G[L[B]] =
      CatsTraverse[L].traverse(la)(f)
  }