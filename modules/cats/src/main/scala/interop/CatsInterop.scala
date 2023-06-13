package scalka.interop.cats

import cats.arrow.Category
import cats.~>
import cats.arrow.FunctionK
import cats.{Functor, Monad => CatsMonad}
import scalka.kernel.{Cat, Morphism, Endofunctor, Monad}
import scalka.kernel.types._
import cats.syntax.compose._
import scalka.kernel.Nat
import scalka.syntax.functionK._
import scalka.kernel.toMorphism

given Cat[AnyK, ScalK, ~>] = new Cat[AnyK, ScalK, ~>] {
  def compose[F[_], G[_], H[_]](f: G -> H, g: F -> G): F -> H =
    Morphism(g.domain, f.arrow compose g.arrow, f.codomain)

  def id[F[_]](ob: ScalK[F]): F -> F =
    Morphism(ob, FunctionK.id[F], ob)
}

given [Arr[_, _]: Category]: Cat[Any, Scal, Arr] = new Cat[Any, Scal, Arr]  {
  def compose[A, B, C](f: B -> C, g: A -> B): A -> C =
    Morphism(g.domain, g.arrow >>> f.arrow, f.codomain)
  def id[A](ob: Scal[A]): A -> A = Morphism(ob, Category[Arr].id[A], ob)
}

given [F[_]: Functor]: Endofunctor[Any, Scal, Function, F] =
  new Endofunctor[Any, Scal, Function, F] {
    def fmap[A, B](f: A -> B): F[A] ~> F[B] =
      Morphism(summon, Functor[F].fmap(_)(f.arrow), summon)
  }

given [F[_]: CatsMonad]: Monad[Any, Scal, Function, F] =
  new Monad[Any, Scal, Function, F] {
    type Pure[A] = Morphism[Any, Scal, Function, A, F[A]]
    type Flatten[A] = Morphism[Any, Scal, Function, (F o F)[A], F[A]]

    val category: Cat[Any, Scal, Function] = summon
    val pure: Transform[IdK[Any], F] =
      val funK = functionK[Any, Scal, Pure](_ => Morphism(summon, CatsMonad[F].pure, summon))
      Nat(funK)
    
    val flatten: Transform[F o F, F] = new Transform[F o F, F] {
      def apply[A](ob: Scal[A]): (F o F)[A] ~> F[A] =
        val arrow: (F o F)[A] => F[A] = CatsMonad[F].flatten[A]
        arrow.toMorphism[Scal](summon, summon)
    }

    def fmap[A, B](f: A -> B): F[A] -> F[B] =
      val arrow: F[A] => F[B] = CatsMonad[F].map(_)(f.arrow)
      arrow.toMorphism[Scal](summon, summon)
  }