package scalka.kernel

import scalka.kernel.types.IdK
import scalka.instances.adjunction.ScalInstances

trait Adjunction[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  R[A <: S] <: D, L[A <: D] <: S
] {
  type RL[A <: D] = R[L[A]]
  type LR[A <: S] = L[R[A]]

  val S: Category[S, SOb, ->]
  val D: Category[D, DOb, ~>]
   
  val R: Functor[S, SOb, ->, D, DOb, ~>, R]
  val L: Functor[D, DOb, ~>, S, SOb, ->, L]

  def left[A <: D: DOb, B <: S: SOb](f: L[A] -> B): A ~> R[B]
  def right[A <: D: DOb, B <: S: SOb](f: A ~> R[B]): L[A] -> B
  

  def unit: Nat[D, DOb, D, DOb, ~>, IdK[D], RL] =
    new Nat[D, DOb, D, DOb, ~>, IdK[D], RL] {
      def domain[A <: D: DOb]: DOb[A] =
        summon

      def apply[A <: D: DOb]: A ~> RL[A] =
        given la: SOb[L[A]] = L[A]
        left(L.fmap(D.id[A]))

      def codomain[A <: D: DOb]: DOb[RL[A]] =
        given la: SOb[L[A]] = L[A]
        R[L[A]]
    }

  def counit: Nat[S, SOb, S, SOb, ->, LR, IdK[S]] =
    new Nat[S, SOb, S, SOb, ->, LR, IdK[S]] {
      def domain[A <: S: SOb]: SOb[LR[A]] =
        given ra: DOb[R[A]] = R[A]
        L[R[A]]

      def apply[A <: S: SOb]: LR[A] -> A =
        given ra: DOb[R[A]] = R[A]
        right(R.fmap(S.id[A]))

      def codomain[A <: S: SOb]: SOb[A] =
        summon

    }

  def monad: Monad[D, DOb, ~>, RL] =
    new Monad[D, DOb, ~>, RL]  { self =>
      val category = D

      val pure: Transform[IdK[D], RL] = unit

      def  apply[A <: D: DOb]: DOb[RL[A]] =
        given la: SOb[L[A]] = L[A]
        R[L[A]]

      val flatten: Transform[[A <: D] =>> RL[RL[A]], RL] =
        new Transform[[A <: D] =>> RL[RL[A]], RL] {
          def domain[A <: D: DOb]: DOb[RL[RL[A]]] =
            given rla: DOb[RL[A]] = self[A]
            self[RL[A]]

          def apply[A <: D: DOb]: RL[RL[A]] ~> RL[A] =
            given rla: DOb[RL[A]] = self[A]
            given la: SOb[L[A]] = L[A]
            given lrla: SOb[L[RL[A]]] = L[R[L[A]]]
            R.fmap(right(fmap(D.id[A])))

          def codomain[A <: D: DOb]: DOb[RL[A]] = self[A]
        }

      def fmap[A <: D: DOb, B <: D: DOb](f: A ~> B): RL[A] ~> RL[B] =
        given la: SOb[L[A]] = L[A]
        given lb: SOb[L[B]] = L[B]
        R.fmap(L.fmap(f))
    }

  def comonad: Comonad[S, SOb, ->, LR] =
    new Comonad[S, SOb, ->, LR] { self =>
      val category = S
      val extract: Transform[LR, IdK[S]] = counit

      def apply[A <: S: SOb]: SOb[LR[A]] =
        given ra: DOb[R[A]] = R[A]
        L[R[A]]

      val coflatten: Transform[LR, [A <: S] =>> LR[LR[A]]] =
        new Transform[LR, [A <: S] =>> LR[LR[A]]] {
          def domain[A <: S: SOb]: SOb[LR[A]] = self[A]

          def apply[A <: S: SOb]: LR[A] -> LR[LR[A]] =
            given ra: DOb[R[A]] = R[A]
            given lra: SOb[LR[A]] = self[A]
            given rlra: DOb[R[LR[A]]] = R[LR[A]]
            L.fmap(left(fmap(S.id[A])))

          def codomain[A <: S: SOb]: SOb[LR[LR[A]]] =
            given lra: SOb[LR[A]] = self[A]
            self[LR[A]]
        }

      def fmap[A <: S: SOb, B <: S: SOb](f: A -> B): L[R[A]] -> L[R[B]] =
        given ra: DOb[R[A]] = R[A]
        given rb: DOb[R[B]] = R[B]
        L.fmap(R.fmap(f))
    }
}

trait AdjunctionInstances extends ScalInstances 

object Adjunction extends AdjunctionInstances
