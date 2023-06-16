package scalka.interop.cats

import cats.Applicative
import cats.arrow.{Category => CatsCategory}
import cats.arrow.{FunctionK => CatsFunctionK}
import cats.{Functor => CatsFunctor}
import cats.{Monad => CatsMonad}
import cats.{Traverse => CatsTraverse}
import cats.~>
import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.functionK.function2K

given ScalCategory2K[~>] = new Category[Any2K, Scal2K, ~>] {
  def compose[F[_]: Scal2K, G[_]: Scal2K, H[_]: Scal2K](f: CatsFunctionK[G, H], g: CatsFunctionK[F, G]): CatsFunctionK[F, H] =
    f compose g

  def id[F[_]: Scal2K]: CatsFunctionK[F, F] = CatsFunctionK.id[F]
}

def forgetScalkaFunctorK: Functor[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] =
  new Functor[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] {
    def fmap[F[_]: Scal2K, G[_]: Scal2K](f: Function2K[F, G]): F ~> G =
      scalka2catsFunctionK(f)

    def apply[F[_]: Scal2K]: Scal2K[F] =
      summon
  }

given [->[_, _]: CatsCategory]: ScalCategory1K[->] = new Category[Any, Scal, ->]  {

  def compose[A: Scal, B: Scal, C: Scal](f: B -> C, g: A -> B): A -> C =
    g >>> f

  def id[A: Scal]: A -> A =
    CatsCategory[->].id[A]
}

def cats2scalkaFunctionK[F[_], G[_]](f: CatsFunctionK[F, G]): FunctionK[Any, F, G] =
  function2K[F, G](fa => f(fa))

def scalka2catsFunctionK[F[_], G[_]](f: FunctionK[Any, F, G]): CatsFunctionK[F, G] =
  new CatsFunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa)
  }

given [F[_]: CatsFunctor]: ScalEndofunctor1K[F] =
  new ScalEndofunctor1K[F] {
    def fmap[A: Scal, B: Scal](f: A => B): F[A] => F[B] =
      CatsFunctor[F].fmap(_)(f)
    
    def apply[A: Scal]: Scal[F[A]] =
      summon
  }

given [F[_]: CatsMonad]: ScalMonad1K[F] =
  new ScalMonad1K[F]{
    val category: Category[Any, Scal, Function] = summon

    def apply[A: Scal]: Scal[F[A]] = summon

    val pure: Transform[IdK[Any], F] =
      new Transform[IdK[Any], F] {
        def domain[A: Scal]: Scal[A] = summon
        def apply[A: Scal]: A => F[A] = CatsMonad[F].pure
        def codomain[A: Scal]: Scal[F[A]] = summon
      }
    
    val flatten: Transform[F o F, F] =
      new Transform[F o F, F] {
        def domain[A: Scal]: Scal[(F o F)[A]] = summon
        def apply[A: Scal]: (F o F)[A] => F[A] = CatsMonad[F].flatten
        def codomain[A: Scal]: Scal[F[A]] = summon
      }

    def fmap[A: Scal, B: Scal](f: A => B): F[A] => F[B] =
      CatsMonad[F].map(_)(f)
  }

given [L[_]: CatsTraverse, G[_]: Applicative]: ScalTraverse1K[L, G] =
  new ScalTraverse1K[L, G] {
    def fmap[A: Scal, B: Scal](f: Kleisli[Any, Function, G, A, B]): Kleisli[Any, Function, G, L[A], L[B]] =
      (la: L[A]) => CatsTraverse[L].traverse(la)(f)
    def apply[A: Scal]: Scal[L[A]] = summon
  }