package scalka.interop.cats

import cats.arrow.Category
import cats.~>
import cats.arrow.FunctionK
import cats.Functor
import scalka.kernel.{Cat, Morphism, Endofunctor}
import scalka.kernel.types._
import cats.syntax.compose._

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