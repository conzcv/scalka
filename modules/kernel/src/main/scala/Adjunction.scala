package scalka.kernel

import scalka.kernel.types._
import scalka.instances.adjunction.AdjunctionInstances

trait Adjunction[SOb[_], ->[_, _], DOb[_], ~>[_, _], R[_], L[_]] {
  val leftCategory: Category[SOb, ->]
  val rightCategory: Category[DOb, ~>]
   
  val right: Functor[SOb, ->, DOb, ~>, R]
  val left: Functor[DOb, ~>, SOb, ->, L]

  def leftAdjunct[A: DOb, B: SOb](f: L[A] -> B): A ~> R[B]
  def rightAdjunct[A: DOb, B: SOb](f: A ~> R[B]): L[A] -> B
  

  def unit: Nat[DOb, DOb, ~>, Id, R o L] =
    new Nat[DOb, DOb, ~>, Id, R o L] {
      def domain[A: DOb]: DOb[A] =
        summon

      def relation[A: DOb]: A ~> (R o L)[A] =
        given la: SOb[L[A]] = left[A]
        leftAdjunct(left.fmap(rightCategory.id[A]))

      def codomain[A: DOb]: DOb[(R o L)[A]] =
        given la: SOb[L[A]] = left[A]
        right[L[A]]
    }

  def counit: Nat[SOb, SOb, ->, L o R, Id] =
    new Nat[SOb, SOb, ->, L o R, Id] {
      def domain[A: SOb]: SOb[(L o R)[A]] =
        given ra: DOb[R[A]] = right[A]
        left[R[A]]

      def relation[A: SOb]: (L o R)[A] -> A =
        given ra: DOb[R[A]] = right[A]
        rightAdjunct(right.fmap(leftCategory.id[A]))

      def codomain[A: SOb]: SOb[A] =
        summon

    }

  def monad: Monad[DOb, ~>, R o L] =
    new Monad[DOb, ~>, R o L]  { self =>
      val category = rightCategory
      val pure: Transform[Id, R o L] = unit

      val flatten: Transform[R o L o R o L, R o L] =
        new Transform[R o L o R o L, R o L] {
          def domain[A: DOb]: DOb[(R o L o R o L)[A]] =
            given rla: DOb[(R o L)[A]] = self[A]
            self[(R o L)[A]]

          def relation[A: DOb]: (R o L o R o L)[A] ~> (R o L)[A] =
            given rla: DOb[(R o L)[A]] = self[A]
            given la: SOb[L[A]] = left[A]
            given lrla: SOb[(L o R o L)[A]] = left[R[L[A]]]
            right.fmap(rightAdjunct(fmap(rightCategory.id[A])))

          def codomain[A: DOb]: DOb[(R o L)[A]] = self[A]
        }

      def fmap[A: DOb, B: DOb](f: A ~> B): (R o L)[A] ~> (R o L)[B] =
        given la: SOb[L[A]] = left[A]
        given lb: SOb[L[B]] = left[B]
        right.fmap(left.fmap(f))
    }

  def comonad: Comonad[SOb, ->, L o R] =
    new Comonad[SOb, ->, L o R] { self =>
      val category = leftCategory
      val extract: Transform[L o R, Id] = counit
      val duplicate: Transform[L o R, L o R o L o R] =
        new Transform[L o R, L o R o L o R] {
          def domain[A: SOb]: SOb[(L o R)[A]] = self[A]

          def relation[A: SOb]: (L o R)[A] -> (L o R o L o R)[A] =
            given ra: DOb[R[A]] = right[A]
            given lra: SOb[(L o R)[A]] = self[A]
            given rlra: DOb[(R o L o R)[A]] = right[(L o R)[A]]
            left.fmap(leftAdjunct(fmap(leftCategory.id[A])))

          def codomain[A: SOb]: SOb[(L o R o L o R)[A]] =
            given lra: SOb[(L o R)[A]] = self[A]
            self[(L o R)[A]]
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
