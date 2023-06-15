package scalka.kernel

import scalka.kernel.types._
import scalka.syntax.functionK._

trait Category[K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K]] {
  type ->[A <: K, B <: K] = Morphism[K, Ob, Rel, A, B]

  def idRelation[A <: K: Ob]: Rel[A, A]

  def compose[A <: K, B <: K, C <: K](f: B -> C, g: A -> B): A -> C 
  final def id[A <: K: Ob]: A -> A = Morphism.fromRelation(idRelation)
}

extension [Kind <: AnyKind, Ob[A <: Kind], A <: Kind](obj: Ob[A]) {
  def id[Arr[A <: Kind, B <: Kind]](using C: Category[Kind, Ob, Arr]) = C.id(obj)
}

trait WeakCategory[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K]] extends Category[K, Ob, Arr] {
  def composeRelations[A <: K, B <: K, C <: K](f: Arr[B, C], g: Arr[A, B]): Arr[A, C]

  final def compose[A <: K, B <: K, C <: K](f: B -> C, g: A -> B): A -> C =
    Morphism(g.domain, composeRelations(f.relation, g.relation), f.codomain)
}

sealed trait CatInstances {

  given [K <: AnyKind]: CatK[K] =
    new WeakCategory[AnyK[K], ScalKCons[K], FunctionKCons[K]] {
      def idRelation[F[A <: K]: ScalKCons[K]]: FunctionK[K, F, F] = FunctionK.id[K, F]
      def composeRelations[F[A <: K], G[A <: K], H[A <: K]](f: FunctionK[K, G, H], g: FunctionK[K, F, G]): FunctionK[K, F, H] =
        g andThen f
    }

  given Category[Any, Scal, Function] = new WeakCategory[Any, Scal, Function] {
    def composeRelations[A, B, C](f: B => C, g: A => B): A => C =
      f compose g
    def idRelation[A: Scal]: A => A = identity
  }

  given Category[Any, Scal, <:<] = new WeakCategory[Any, Scal, <:<] {
    def composeRelations[A, B, C](f: B <:< C, g: A <:< B): A <:< C =
      f compose g

    def idRelation[A: Scal]: A <:< A = summon
  }

  given [
    S <: AnyKind, SOb[A <: S], SRel[A <: S, B <: S],
    D <: AnyKind, DOb[A <: D], DRel[A <: D, B <: D]
  ](using S: Category[S, SOb, SRel], D: Category[D, DOb, DRel]): FunctorCat[S, SOb, SRel, D, DOb, DRel] =
    type FunctorOb[F[A <: S] <: D] = Functor[S, SOb, SRel, D, DOb, DRel, F]
    new WeakCategory[
      [A <: S] =>> D,
      [F[A <: S] <: D] =>> Functor[S, SOb, SRel, D, DOb, DRel, F],
      [F[A <: S] <: D, G[A <: S] <: D] =>> Nat[S, SOb, D, DOb, DRel, F, G]
    ] {
      def composeRelations[
        F[A <: S] <: D,
        G[A <: S] <: D,
        H[A <: S] <: D
      ](f: Nat[S, SOb, D, DOb, DRel, G, H], g: Nat[S, SOb, D, DOb, DRel, F, G]): Nat[S, SOb, D, DOb, DRel, F, H] =
        type Transformation[A <: S] = Morphism[D, DOb, DRel, F[A], H[A]]
        Nat(functionK[S, SOb, Transformation](ob => g(ob) >> f(ob)))

      def idRelation[F[A <: S] <: D: FunctorOb]: Nat[S, SOb, D, DOb, DRel, F, F] =
        type Transformation[A <: S] = Morphism[D, DOb, DRel, F[A], F[A]]
        Nat(functionK[S, SOb, Transformation](ob => summon[FunctorOb[F]].fmap(ob.id)))
    }

  given [K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], F[A <: K] <: K](using C: Category[K, Ob, Rel], M: Monad[K, Ob, Rel, F]): KleisliCat[K, Ob, Rel, F] =
    new KleisliCat[K, Ob, Rel, F] {
      def compose[A <: K, B <: K, C <: K](f: B -> C, g: A -> B): A -> C =
        val relation = g.relation >> M.flatMap(f.relation)(f.codomain)
        Morphism(g.domain, relation, f.codomain)

      def idRelation[A <: K: Ob]: Kleisli[K, Ob, Rel, F, A, A] = M.pure[A]
    }
}

object Category extends CatInstances {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]](using C: Category[K, Ob, ->]): Category[K, Ob, ->] = C
}