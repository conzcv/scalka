package scalka.kernel

import scalka.kernel.types._
import scalka.instances.adjunction.AdjunctionInstances

trait Adjunction[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  R[A <: S] <: D, L[A <: D] <: S
] {
  type RL[A <: D] = R[L[A]]
  type LR[A <: S] = L[R[A]]

  val leftCategory: Category[S, SOb, ->]
  val rightCategory: Category[D, DOb, ~>]
   
  val right: Functor[S, SOb, ->, D, DOb, ~>, R]
  val left: Functor[D, DOb, ~>, S, SOb, ->, L]

  def leftAdjunct[A <: D: DOb, B <: S: SOb](f: L[A] -> B): A ~> R[B]
  def rightAdjunct[A <: D: DOb, B <: S: SOb](f: A ~> R[B]): L[A] -> B
  

  def unit: Nat[D, DOb, D, DOb, ~>, IdK[D], RL] =
    new Nat[D, DOb, D, DOb, ~>, IdK[D], RL] {
      def domain[A <: D: DOb]: DOb[A] =
        summon

      def relation[A <: D: DOb]: A ~> RL[A] =
        given la: SOb[L[A]] = left[A]
        leftAdjunct(left.fmap(rightCategory.id[A]))

      def codomain[A <: D: DOb]: DOb[RL[A]] =
        given la: SOb[L[A]] = left[A]
        right[L[A]]
    }

  def counit: Nat[S, SOb, S, SOb, ->, LR, IdK[S]] =
    new Nat[S, SOb, S, SOb, ->, LR, IdK[S]] {
      def domain[A <: S: SOb]: SOb[LR[A]] =
        given ra: DOb[R[A]] = right[A]
        left[R[A]]

      def relation[A <: S: SOb]: LR[A] -> A =
        given ra: DOb[R[A]] = right[A]
        rightAdjunct(right.fmap(leftCategory.id[A]))

      def codomain[A <: S: SOb]: SOb[A] =
        summon

    }

  def monad: Monad[D, DOb, ~>, RL] =
    new Monad[D, DOb, ~>, RL]  { self =>
      val category = rightCategory
      val pure: Transform[IdK[D], RL] = unit

      def  apply[A <: D: DOb]: DOb[RL[A]] =
        given la: SOb[L[A]] = left[A]
        right[L[A]]

      val flatten: Transform[[A <: D] =>> RL[RL[A]], RL] =
        new Transform[[A <: D] =>> RL[RL[A]], RL] {
          def domain[A <: D: DOb]: DOb[RL[RL[A]]] =
            given rla: DOb[RL[A]] = self[A]
            self[RL[A]]

          def relation[A <: D: DOb]: RL[RL[A]] ~> RL[A] =
            given rla: DOb[RL[A]] = self[A]
            given la: SOb[L[A]] = left[A]
            given lrla: SOb[L[RL[A]]] = left[R[L[A]]]
            right.fmap(rightAdjunct(fmap(rightCategory.id[A])))

          def codomain[A <: D: DOb]: DOb[RL[A]] = self[A]
        }

      def fmap[A <: D: DOb, B <: D: DOb](f: A ~> B): RL[A] ~> RL[B] =
        given la: SOb[L[A]] = left[A]
        given lb: SOb[L[B]] = left[B]
        right.fmap(left.fmap(f))
    }

  def comonad: Comonad[S, SOb, ->, LR] =
    new Comonad[S, SOb, ->, LR] { self =>
      val category = leftCategory
      val extract: Transform[LR, IdK[S]] = counit

      def apply[A <: S: SOb]: SOb[LR[A]] =
        given ra: DOb[R[A]] = right[A]
        left[R[A]]

      val duplicate: Transform[LR, [A <: S] =>> LR[LR[A]]] =
        new Transform[LR, [A <: S] =>> LR[LR[A]]] {
          def domain[A <: S: SOb]: SOb[LR[A]] = self[A]

          def relation[A <: S: SOb]: LR[A] -> LR[LR[A]] =
            given ra: DOb[R[A]] = right[A]
            given lra: SOb[LR[A]] = self[A]
            given rlra: DOb[R[LR[A]]] = right[LR[A]]
            left.fmap(leftAdjunct(fmap(leftCategory.id[A])))

          def codomain[A <: S: SOb]: SOb[LR[LR[A]]] =
            given lra: SOb[LR[A]] = self[A]
            self[LR[A]]
        }

      def fmap[A <: S: SOb, B <: S: SOb](f: A -> B): L[R[A]] -> L[R[B]] =
        given ra: DOb[R[A]] = right[A]
        given rb: DOb[R[B]] = right[B]
        left.fmap(right.fmap(f))
    }
}

object Adjunction extends AdjunctionInstances
