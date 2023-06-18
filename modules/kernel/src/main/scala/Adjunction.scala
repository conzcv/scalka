package scalka.kernel

import scalka.kernel.types._
import scalka.instances.adjunction.AdjunctionInstances

trait Adjunction[SOb[_], ->[_, _], DOb[_], ~>[_, _], R[_], L[_]] {
  type RL[A] = R[L[A]]
  type LR[A] = L[R[A]]

  val leftCategory: Category[SOb, ->]
  val rightCategory: Category[DOb, ~>]
   
  val right: Functor[SOb, ->, DOb, ~>, R]
  val left: Functor[DOb, ~>, SOb, ->, L]

  def leftAdjunct[A: DOb, B: SOb](f: L[A] -> B): A ~> R[B]
  def rightAdjunct[A: DOb, B: SOb](f: A ~> R[B]): L[A] -> B
  

  def unit: Nat[DOb, DOb, ~>, Id, RL] =
    new Nat[DOb, DOb, ~>, Id, RL] {
      def domain[A: DOb]: DOb[A] =
        summon

      def relation[A: DOb]: A ~> RL[A] =
        given la: SOb[L[A]] = left[A]
        leftAdjunct(left.fmap(rightCategory.id[A]))

      def codomain[A: DOb]: DOb[RL[A]] =
        given la: SOb[L[A]] = left[A]
        right[L[A]]
    }

  def counit: Nat[SOb, SOb, ->, LR, Id] =
    new Nat[SOb, SOb, ->, LR, Id] {
      def domain[A: SOb]: SOb[LR[A]] =
        given ra: DOb[R[A]] = right[A]
        left[R[A]]

      def relation[A: SOb]: LR[A] -> A =
        given ra: DOb[R[A]] = right[A]
        rightAdjunct(right.fmap(leftCategory.id[A]))

      def codomain[A: SOb]: SOb[A] =
        summon

    }

  def monad: Monad[DOb, ~>, RL] =
    new Monad[DOb, ~>, RL]  { self =>
      val category = rightCategory
      val pure: Transform[Id, RL] = unit

      val flatten: Transform[[A] =>> RL[RL[A]], RL] =
        new Transform[[A] =>> RL[RL[A]], RL] {
          def domain[A: DOb]: DOb[RL[RL[A]]] =
            given rla: DOb[RL[A]] = self[A]
            self[RL[A]]

          def relation[A: DOb]: RL[RL[A]] ~> RL[A] =
            given rla: DOb[RL[A]] = self[A]
            given la: SOb[L[A]] = left[A]
            given lrla: SOb[L[RL[A]]] = left[R[L[A]]]
            right.fmap(rightAdjunct(fmap(rightCategory.id[A])))

          def codomain[A: DOb]: DOb[RL[A]] = self[A]
        }

      def fmap[A: DOb, B: DOb](f: A ~> B): RL[A] ~> RL[B] =
        given la: SOb[L[A]] = left[A]
        given lb: SOb[L[B]] = left[B]
        right.fmap(left.fmap(f))
    }

  def comonad: Comonad[SOb, ->, LR] =
    new Comonad[SOb, ->, LR] { self =>
      val category = leftCategory
      val extract: Transform[LR, Id] = counit
      val duplicate: Transform[LR, [A] =>> LR[LR[A]]] =
        new Transform[LR, [A] =>> LR[LR[A]]] {
          def domain[A: SOb]: SOb[LR[A]] = self[A]

          def relation[A: SOb]: LR[A] -> LR[LR[A]] =
            given ra: DOb[R[A]] = right[A]
            given lra: SOb[LR[A]] = self[A]
            given rlra: DOb[R[LR[A]]] = right[LR[A]]
            left.fmap(leftAdjunct(fmap(leftCategory.id[A])))

          def codomain[A: SOb]: SOb[LR[LR[A]]] =
            given lra: SOb[LR[A]] = self[A]
            self[LR[A]]
        }

      def fmap[A: SOb, B: SOb](f: A -> B): L[R[A]] -> L[R[B]] =
        given ra: DOb[R[A]] = right[A]
        given rb: DOb[R[B]] = right[B]
        left.fmap(right.fmap(f))
    }
}

trait SetAdjunction[R[_], L[_]] extends Adjunction[Scal, Function, Scal, Function, R, L] {
  final val leftCategory = summon
  final val rightCategory = summon

  def leftAdj[A, B](a: A)(f: L[A] => B): R[B]
  def rightAdj[A, B](la: L[A])(f: A => R[B]): B

  def leftAdjunct[A: Scal, B: Scal](f: L[A] => B): A => R[B] =
    a => leftAdj(a)(f)

  def rightAdjunct[A: Scal, B: Scal](f: A => R[B]): L[A] => B =
    la => rightAdj(la)(f)
}

object Adjunction extends AdjunctionInstances
