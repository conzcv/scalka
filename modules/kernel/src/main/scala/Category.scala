package scalka.kernel

import scalka.kernel.types._
import scalka.syntax.functionK._

trait Category[Kind <: AnyKind, Ob[A <: Kind], Rel[A <: Kind, B <: Kind]] {
  type Object[A <: Kind] = Ob[A]
  type Relation[A <: Kind, B <: Kind] = Rel[A, B]
  type ->[A <: Kind, B <: Kind] = Morphism[Kind, Ob, Rel, A, B]

  def idRelation[A <: Kind](ob: Object[A]): Rel[A, A]

  def compose[A <: Kind, B <: Kind, C <: Kind](f: B -> C, g: A -> B): A -> C 
  final def id[A <: Kind](ob: Object[A]): A -> A = Morphism(ob, idRelation(ob), ob)
}

extension [Kind <: AnyKind, Ob[A <: Kind], A <: Kind](obj: Ob[A]) {
  def id[Arr[A <: Kind, B <: Kind]](using C: Category[Kind, Ob, Arr]) = C.id(obj)
}

trait SimpleCategory[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K]] extends Category[K, Ob, Arr] {
  def composeRelations[A <: K, B <: K, C <: K](f: Arr[B, C], g: Arr[A, B]): Arr[A, C]

  final def compose[A <: K, B <: K, C <: K](f: B -> C, g: A -> B): A -> C =
    Morphism(g.domain, composeRelations(f.arrow, g.arrow), f.codomain)
}

sealed trait CatInstances {

  given [K <: AnyKind]: CatK[K] =
    new SimpleCategory[AnyK[K], ScalKCons[K], FunctionKCons[K]] {
      def idRelation[F[A <: K]](ob: Object[F]): FunctionK[K, F, F] = FunctionK.id[K, F]
      def composeRelations[F[A <: K], G[A <: K], H[A <: K]](f: FunctionK[K, G, H], g: FunctionK[K, F, G]): FunctionK[K, F, H] =
        g andThen f
    }

  given Category[Any, Scal, Function] = new SimpleCategory[Any, Scal, Function] {
    def composeRelations[A, B, C](f: B => C, g: A => B): A => C =
      f compose g
    def idRelation[A](ob: Object[A]): A => A = identity
  }

  given Category[Any, Scal, <:<] = new SimpleCategory[Any, Scal, <:<] {
    def composeRelations[A, B, C](f: B <:< C, g: A <:< B): A <:< C =
      f compose g

    def idRelation[A](ob: Object[A]): A <:< A = summon
  }

  given [
    SKind <: AnyKind, SOb[A <: SKind], SRel[A <: SKind, B <: SKind],
    DKind <: AnyKind, DOb[A <: DKind], DRel[A <: DKind, B <: DKind]
  ](using S: Category[SKind, SOb, SRel], D: Category[DKind, DOb, DRel]): FunctorCat[SKind, SOb, SRel, DKind, DOb, DRel] =
    new SimpleCategory[
      [A <: SKind] =>> DKind,
      [F[A <: SKind] <: DKind] =>> Functor[SKind, SOb, SRel, DKind, DOb, DRel, F],
      [F[A <: SKind] <: DKind, G[A <: SKind] <: DKind] =>> Nat[SKind, SOb, DKind, DOb, DRel, F, G]
    ] {
      def composeRelations[
        F[A <: SKind] <: DKind,
        G[A <: SKind] <: DKind,
        H[A <: SKind] <: DKind
      ](f: Nat[SKind, SOb, DKind, DOb, DRel, G, H], g: Nat[SKind, SOb, DKind, DOb, DRel, F, G]): Nat[SKind, SOb, DKind, DOb, DRel, F, H] =
        type Transformation[A <: SKind] = Morphism[DKind, DOb, DRel, F[A], H[A]]
        Nat(functionK[SKind, SOb, Transformation](ob => g(ob) >> f(ob)))

      def idRelation[F[A <: SKind] <: DKind](obj: Object[F]): Nat[SKind, SOb, DKind, DOb, DRel, F, F] =
        type Transformation[A <: SKind] = Morphism[DKind, DOb, DRel, F[A], F[A]]
        Nat(functionK[SKind, SOb, Transformation](ob => obj.fmap(ob.id)))
    }

  given [K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], F[A <: K] <: K](using C: Category[K, Ob, Rel], M: Monad[K, Ob, Rel, F]): KleisliCat[K, Ob, Rel, F] =
    new KleisliCat[K, Ob, Rel, F] {
      def compose[A <: K, B <: K, C <: K](f: B -> C, g: A -> B): A -> C =
        val arrow = g.arrow >> M.flatMap(f.codomain)(f.arrow)
        Morphism(g.domain, arrow, f.codomain)

      def idRelation[A <: K](ob: Ob[A]): Kleisli[K, Ob, Rel, F, A, A] = M.pure(ob)
    }
}

object Category extends CatInstances {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]](using C: Category[K, Ob, ->]): Category[K, Ob, ->] = C
}