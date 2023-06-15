package scalka.kernel

import scalka.kernel.Morphism
import scalka.kernel.types.{IdK, Scal, ScalEndofunctor1K, ScalAdjunction1K}
import scalka.syntax.functionK.functionK

trait Adjunction[
  S <: AnyKind, SOb[A <: S], SRel[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], DRel[A <: D, B <: D],
  R[A <: S] <: D, L[A <: D] <: S
] {
  type ->>[A <: S, B <: S] = Morphism[S, SOb, SRel, A, B]
  type ~>>[A <: D, B <: D] = Morphism[D, DOb, DRel, A, B]
  type Unit[A <: D] = A ~>> R[L[A]]
  type Counit[A <: S] = L[R[A]] ->> A
  type RL[A <: D] = R[L[A]]
  type LR[A <: S] = L[R[A]]

  val S: Category[S, SOb, SRel]
  val D: Category[D, DOb, DRel]
   
  val R: Functor[S, SOb, SRel, D, DOb, DRel, R]
  val L: Functor[D, DOb, DRel, S, SOb, SRel, L]

  def left[A <: D, B <: S](ob: DOb[A])(f: L[A] ->> B): A ~>> R[B]
  def right[A <: D, B <: S](ob: SOb[B])(f: A ~>> R[B]): L[A] ->> B
  

  def unit: Nat[D, DOb, D, DOb, DRel, IdK[D], RL] =
    val funk = functionK[D, DOb, Unit](ob => left(ob)(L.fmap(D.id(ob))))
    Nat[D, DOb, D, DOb, DRel, IdK[D], RL](funk)

  def counit: Nat[S, SOb, S, SOb, SRel, LR, IdK[S]] =
    val funk = functionK[S, SOb, Counit](ob => right(ob)(R.fmap(S.id(ob))))
    Nat[S, SOb, S, SOb, SRel, LR, IdK[S]](funk)

  def monad: Monad[D, DOb, DRel, RL] =
    new Monad[D, DOb, DRel, RL]  {
      val category = D
      val pure: Transform[IdK[D], RL] = unit

      val flatten: Transform[[A <: D] =>> RL[RL[A]], RL] =
        new Transform[[A <: D] =>> RL[RL[A]], RL] {
          def apply[A <: D](ob: DOb[A]): RL[RL[A]] ~> RL[A] =
            val id = D.id(ob)
            R.fmap(right(L.fmap(id).domain)(fmap(id)))
        }

      def fmap[A <: D, B <: D](f: A ~>> B): RL[A] ~>> RL[B] =
        R.fmap(L.fmap(f))
    }

  def comonad: Comonad[S, SOb, SRel, LR] =
    new Comonad[S, SOb, SRel, LR] {
      val category = S
      val extract: Transform[LR, IdK[S]] = counit

      val coflatten: Transform[LR, [A <: S] =>> LR[LR[A]]] =
        new Transform[LR, [A <: S] =>> LR[LR[A]]]  {
          def apply[A <: S](ob: SOb[A]): LR[A] ~> LR[LR[A]] =
            val id = S.id(ob)
            L.fmap(left(R.fmap(id).codomain)(fmap(id)))
        }

      def fmap[A <: S, B <: S](f: A ->> B): L[R[A]] ->> L[R[B]] =
        L.fmap(R.fmap(f))
    }
}

object Adjunction {
  given [X]: ScalAdjunction1K[X => _, (X, _)] =
    new ScalAdjunction1K[X => _, (X, _)] {
      def left[A, B](ob: Scal[A])(f: (X, A) ->> B): A ~>> (X => B) =
        Morphism.fromRelation(a => f.arrow(_, a))
      def right[A, B](ob: Scal[B])(f: A ~>> (X => B)): (X, A) ->> B =
        Morphism.fromRelation((x, a) => f.arrow(a)(x))
      val S = summon
      val D = summon
      val L = new ScalEndofunctor1K[(X, _)] {
        def fmap[A, B](f: A -> B): (X, A) ~> (X, B) =
          Morphism.fromRelation((x, a) => (x, f.arrow(a)))
      }
      val R = new ScalEndofunctor1K[(X => _)] {
        def fmap[A, B](f: A -> B): (X => A) ~> (X => B) =
          Morphism.fromRelation(xa => xa andThen f.arrow)
      }
    }
}
