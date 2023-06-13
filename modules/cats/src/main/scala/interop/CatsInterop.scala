package scalka.interop.cats

import cats.arrow.Category
import cats.~>
import cats.arrow.{FunctionK => CatsFunctionK}
import scalka.kernel.FunctionK
import cats.{Functor, Monad => CatsMonad}
import scalka.kernel.{Cat, Morphism, Endofunctor, Monad}
import scalka.kernel.types._
import cats.syntax.compose._
import scalka.kernel.Nat
import scalka.syntax.functionK._
import scalka.kernel.toMorphism

given Cat[AnyK, ScalK, ~>] = new Cat[AnyK, ScalK, ~>] {

  def composeArrows[F[_], G[_], H[_]](f: CatsFunctionK[G, H], g: CatsFunctionK[F, G]): CatsFunctionK[F, H] =
    f compose g

  def id[F[_]](ob: ScalK[F]): F -> F =
    Morphism(ob, CatsFunctionK.id[F], ob)
}

given [Arr[_, _]: Category]: Cat[Any, Scal, Arr] = new Cat[Any, Scal, Arr]  {

  def composeArrows[A, B, C](f: Arr[B, C], g: Arr[A, B]): Arr[A, C] =
    g >>> f
  def id[A](ob: Scal[A]): A -> A = Morphism(ob, Category[Arr].id[A], ob)
}

def cats2scalkaFunctionK[F[_], G[_]](f: CatsFunctionK[F, G]): FunctionK[Any, F, G] =
  functionK[Any, F, G](fa => f(fa))

def scalka2catsFunctionK[F[_], G[_]](f: FunctionK[Any, F, G]): CatsFunctionK[F, G] =
  new CatsFunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa)
  }

given [F[_]: Functor]: Endofunctor[Any, Scal, Function, F] =
  new Endofunctor[Any, Scal, Function, F] {
    def fmap[A, B](f: A -> B): F[A] ~> F[B] =
      Morphism(summon, Functor[F].fmap(_)(f.arrow), summon)
  }

given [F[_]: CatsMonad]: Monad[Any, Scal, Function, F] =
  new Monad[Any, Scal, Function, F] {
    val category: Cat[Any, Scal, Function] = summon
    val pure: Transform[IdK[Any], F] =
      Nat(functionK[Any, Scal, Pure](_ => Morphism(summon, CatsMonad[F].pure, summon)))
    
    val flatten: Transform[F o F, F] =
      Nat(functionK[Any, Scal, Flatten](_ => Morphism(summon, CatsMonad[F].flatten, summon)))

    def fmap[A, B](f: A -> B): F[A] -> F[B] =
      val arrow: F[A] => F[B] = CatsMonad[F].map(_)(f.arrow)
      arrow.toMorphism[Scal](summon, summon)
  }