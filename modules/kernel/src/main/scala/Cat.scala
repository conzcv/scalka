package scalka.kernel

import scalka.kernel.types._
import scalka.syntax.functionK._

trait Cat[Kind <: AnyKind, Ob[A <: Kind], Arr[A <: Kind, B <: Kind]] {
  final type Object[A <: Kind] = Ob[A]
  final type Arrow[A <: Kind, B <: Kind] = Arr[A, B]
  final type ->[A <: Kind, B <: Kind] = Morphism[Kind, Ob, Arr, A, B]

  def composeArrows[A <: Kind, B <: Kind, C <: Kind](f: Arr[B, C], g: Arr[A, B]): Arr[A, C]

  final def compose[A <: Kind, B <: Kind, C <: Kind](f: B -> C, g: A -> B): A -> C =
    Morphism(g.domain, composeArrows(f.arrow, g.arrow), f.codomain)

  def id[A <: Kind](ob: Object[A]): A -> A
}

extension [Kind <: AnyKind, Ob[A <: Kind], A <: Kind](obj: Ob[A]) {
  def id[Arr[A <: Kind, B <: Kind]](using C: Cat[Kind, Ob, Arr]) = C.id(obj)
}

sealed trait CatInstances {
  given Cat[Any, Scal, Function] = new Cat[Any, Scal, Function] {
    def composeArrows[A, B, C](f: B => C, g: A => B): A => C =
      f compose g
    def id[A](ob: Object[A]): A -> A =
      Morphism(ob, identity, ob)
  }

  given Cat[Any, Scal, <:<] = new Cat[Any, Scal, <:<]  {
    def composeArrows[A, B, C](f: B <:< C, g: A <:< B): A <:< C =
      f compose g
    def id[A](ob: Object[A]): A -> A =
      Morphism(ob, summon, ob)
  }

  given [
    SKind <: AnyKind, SOb[A <: SKind], SArr[A <: SKind, B <: SKind],
    DKind <: AnyKind, DOb[A <: DKind], DArr[A <: DKind, B <: DKind]
  ](using S: Cat[SKind, SOb, SArr], D: Cat[DKind, DOb, DArr]): FunctorCat[SKind, SOb, SArr, DKind, DOb, DArr] =
    new FunctorCat[SKind, SOb, SArr, DKind, DOb, DArr] {

      def composeArrows[
        F[A <: SKind] <: DKind,
        G[A <: SKind] <: DKind,
        H[A <: SKind] <: DKind
      ](f: Nat[SKind, SOb, DKind, DOb, DArr, G, H], g: Nat[SKind, SOb, DKind, DOb, DArr, F, G]): Nat[SKind, SOb, DKind, DOb, DArr, F, H] =
        type Transformation[A <: SKind] = Morphism[DKind, DOb, DArr, F[A], H[A]]
        Nat(functionK[SKind, SOb, Transformation](ob => g(ob) >> f(ob)))

      def id[F[A <: SKind] <: DKind](obj: Object[F]): F -> F = {
        type Transformation[A <: SKind] = Morphism[DKind, DOb, DArr, F[A], F[A]]
        val funK = functionK[SKind, SOb, Transformation](ob => obj.fmap(ob.id))
        Morphism(obj, Nat(funK), obj)
      }
    }
}

object Cat extends CatInstances {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]](using C: Cat[K, Ob, ->]): Cat[K, Ob, ->] = C
}