package scalka.interop.cats

import cats.Applicative
import cats.arrow.{Category => CatsCategory}
import cats.arrow.{FunctionK => CatsFunctionK}
import cats.syntax.compose._
import cats.{Functor => CatsFunctor}
import cats.{Monad => CatsMonad}
import cats.{Traverse => CatsTraverse}
import cats.~>
import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.functionK.function2K

given ScalCategory2K[~>] = new SimpleCategory[Any2K, Scal2K, ~>] {
  def composeRelations[F[_], G[_], H[_]](f: CatsFunctionK[G, H], g: CatsFunctionK[F, G]): CatsFunctionK[F, H] =
    f compose g

  def idRelation[F[_]](ob: Object[F]): CatsFunctionK[F, F] = CatsFunctionK.id[F]
}

def forgetScalkaFunctorK: Functor[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] =
  new Functor[Any2K, Scal2K, Function2K, Any2K, Scal2K, CatsFunctionK, [F[A]] =>> F] {
    def fmap[F[_], G[_]](f: Morphism[Any2K, Scal2K, Function2K, F, G]): F ~> G =
      val arrow = scalka2catsFunctionK(f.arrow)
      Morphism.fromRelation(arrow)
  }

given [Arr[_, _]: CatsCategory]: ScalCategory1K[Arr] = new SimpleCategory[Any, Scal, Arr]  {

  def composeRelations[A, B, C](f: Arr[B, C], g: Arr[A, B]): Arr[A, C] =
    g >>> f
  def idRelation[A](ob: Object[A]): Arr[A, A] = CatsCategory[Arr].id[A]
}

def cats2scalkaFunctionK[F[_], G[_]](f: CatsFunctionK[F, G]): FunctionK[Any, F, G] =
  function2K[F, G](fa => f(fa))

def scalka2catsFunctionK[F[_], G[_]](f: FunctionK[Any, F, G]): CatsFunctionK[F, G] =
  new CatsFunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa)
  }

given [F[_]: CatsFunctor]: ScalEndofunctor1K[F] =
  new ScalEndofunctor1K[F] {
    def fmap[A, B](f: A -> B): F[A] ~> F[B] =
      Morphism.fromRelation(CatsFunctor[F].fmap(_)(f.arrow))
  }

given [F[_]: CatsMonad]: ScalMonad1K[F] =
  new ScalMonad1K[F]{
    val category: Category[Any, Scal, Function] = summon
    val pure: Transform[IdK[Any], F] =
      Nat(function2K[Scal, Pure](_ => Morphism.fromRelation(CatsMonad[F].pure)))
    
    val flatten: Transform[F o F, F] =
      Nat(function2K[Scal, Flatten](_ => Morphism.fromRelation(CatsMonad[F].flatten)))

    def fmap[A, B](f: A -> B): F[A] -> F[B] =
      val arrow: F[A] => F[B] = CatsMonad[F].map(_)(f.arrow)
      Morphism.fromRelation(arrow)
  }

given [L[_]: CatsTraverse, G[_]: Applicative]: ScalTraverse1K[L, G] =
  new ScalTraverse1K[L, G] {
    def fmap[A, B](f: A -> B): L[A] -> L[B] =
      val function = (la: L[A]) => CatsTraverse[L].traverse(la)(f.arrow.arrow)
      val arrow: ScalKleisli1K[G, L[A], L[B]] = Morphism.fromRelation(function)
      Morphism.fromRelation(arrow)
  }