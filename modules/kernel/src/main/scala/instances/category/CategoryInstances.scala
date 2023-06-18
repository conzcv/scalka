package scalka.instances.category

import scalka.kernel.types._
import scalka.kernel.{Category, Functor, Nat, Monad, Op}
import scalka.syntax.category.* 

trait CategoryInstances extends ScalInstances with ScalKInstances {

  given [K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]](using C: Category[K, Ob, ->]): Category[K, Ob, [A <: K, B <: K] =>> Op[K, ->, A, B]] =
    new Category[K, Ob, [A <: K, B <: K] =>> Op[K, ->, A, B]] {
      def compose[A <: K: Ob, B <: K: Ob, C <: K: Ob](f: Op[K, ->, B, C], g: Op[K, ->, A, B]): Op[K, ->, A, C] =
        Op(f.opposite >>> g.opposite)
      def id[A <: K: Ob]: Op[K, ->, A, A] = Op(C.id[A])
    }

  given [
    S <: AnyKind, SOb[A <: S], ->[A <: S, B <: S],
    D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D]
  ](using S: Category[S, SOb, ->], D: Category[D, DOb, ~>]): FunctorCat[S, SOb, ->, D, DOb, ~>] =
    type FunctorOb[F[A <: S] <: D] = Functor[S, SOb, ->, D, DOb, ~>, F]
    type NatRelation[F[A <: S] <: D, G[A <: S] <: D] = Nat[S, SOb, D, DOb, ~>, F, G]
    new Category[
      [A <: S] =>> D,
      [F[A <: S] <: D] =>> Functor[S, SOb, ->, D, DOb, ~>, F],
      [F[A <: S] <: D, G[A <: S] <: D] =>> Nat[S, SOb, D, DOb, ~>, F, G]
    ] {
      def compose[
        F[A <: S] <: D: FunctorOb,
        G[A <: S] <: D: FunctorOb,
        H[A <: S] <: D: FunctorOb
      ](f: NatRelation[G, H], g: NatRelation[F, G]): NatRelation[F, H] =
        Nat.compose(f, g)

      def id[F[A <: S] <: D: FunctorOb]: NatRelation[F, F] =
        new NatRelation[F, F] {
          def domain[A <: S: SOb]: DOb[F[A]] =
            summon[FunctorOb[F]].apply[A]

          def relation[A <: S: SOb]: F[A] ~> F[A] =
            summon[FunctorOb[F]].fmap(S.id[A])

          def codomain[A <: S: SOb]: DOb[F[A]] =
            summon[FunctorOb[F]].apply[A]
        }
    }

  given [K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K] <: K](
    using C: Category[K, Ob, ->],
    M: Monad[K, Ob, ->, F]
  ): KleisliCat[K, Ob, ->, F] =
    new KleisliCat[K, Ob, ->, F] {
      def compose[A <: K: Ob, B <: K: Ob, C <: K: Ob](f: Kleisli[K, ->, F, B, C],  g: Kleisli[K, ->, F, A, B]): Kleisli[K, ->, F, A, C] =
        given fb: Ob[F[B]] = M.apply[B]
        given fc: Ob[F[C]] = M.apply[C]
        C.compose(M.bind(f), g)

      def id[A <: K: Ob]: Kleisli[K, ->, F, A, A] =
        M.pure.relation[A]
    }
}

