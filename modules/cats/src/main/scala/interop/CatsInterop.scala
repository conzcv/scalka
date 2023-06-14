package scalka.interop.cats

import cats.arrow.Category
import cats.~>
import cats.arrow.{FunctionK => CatsFunctionK}
import scalka.kernel.FunctionK
import cats.{Functor, Monad => CatsMonad}
import scalka.kernel.{Cat, Morphism, Endofunctor, Monad, SimpleCategory}
import scalka.kernel.types._
import cats.syntax.compose._
import scalka.kernel.Nat
import scalka.syntax.functionK._
import scalka.kernel.Fun
import scalka.kernel.Identity


given ScalCategory2K[~>] = new SimpleCategory[Any2K, Scal2K, ~>] {
  def composeArrows[F[_], G[_], H[_]](f: CatsFunctionK[G, H], g: CatsFunctionK[F, G]): CatsFunctionK[F, H] =
    f compose g

  def idArrow[F[_]]: CatsFunctionK[F, F] = CatsFunctionK.id[F]
}

def forgetScalka: Fun[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] =
  new Fun[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] {
    def fmap[F[_], G[_]](f: Morphism[Any2K, Scal2K, Function2K, F, G]): F ~> G =
      val arrow = scalka2catsFunctionK(f.arrow)
      Morphism.fromArrow(arrow)
  }

given [Arr[_, _]: Category]: ScalCategory1K[Arr] = new SimpleCategory[Any, Scal, Arr]  {

  def composeArrows[A, B, C](f: Arr[B, C], g: Arr[A, B]): Arr[A, C] =
    g >>> f
  def idArrow[A]: Arr[A, A] = Category[Arr].id[A]
}

def cats2scalkaFunctionK[F[_], G[_]](f: CatsFunctionK[F, G]): FunctionK[Any, F, G] =
  functionK[Any, F, G](fa => f(fa))

def scalka2catsFunctionK[F[_], G[_]](f: FunctionK[Any, F, G]): CatsFunctionK[F, G] =
  new CatsFunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa)
  }

given [F[_]: Functor]: ScalEndofunctor1K[F] =
  new ScalEndofunctor1K[F] {
    def fmap[A, B](f: A -> B): F[A] ~> F[B] =
      Morphism(summon, Functor[F].fmap(_)(f.arrow), summon)
  }

given [F[_]: CatsMonad]: ScalMonad1K[F] =
  new ScalMonad1K[F]{
    val category: Cat[Any, Scal, Function] = summon
    val pure: Transform[IdK[Any], F] =
      Nat(functionK[Any, Scal, Pure](_ => Morphism.fromArrow(CatsMonad[F].pure)))
    
    val flatten: Transform[F o F, F] =
      Nat(functionK[Any, Scal, Flatten](_ => Morphism.fromArrow(CatsMonad[F].flatten)))

    def fmap[A, B](f: A -> B): F[A] -> F[B] =
      val arrow: F[A] => F[B] = CatsMonad[F].map(_)(f.arrow)
      Morphism.fromArrow(arrow)
  }