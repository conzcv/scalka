package scalka.kernel

import scalka.kernel.types._
import scalka.instances.adjunction.AdjunctionInstances

trait Adjunction[SOb[_], ->[_, _], DOb[_], ~>[_, _], R[_], L[_]] {
  val leftCategory: Category[SOb, ->]
  val rightCategory: Category[DOb, ~>]
   
  def right: Functor[SOb, ->, DOb, ~>, R]
  def left: Functor[DOb, ~>, SOb, ->, L]

  def leftAdjunct[A: DOb, B: SOb](f: L[A] -> B): A ~> R[B]
  def rightAdjunct[A: DOb, B: SOb](f: A ~> R[B]): L[A] -> B
  

  val unit: Nat[DOb, DOb, ~>, Id, R o L] =
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

  val counit: Nat[SOb, SOb, ->, L o R, Id] =
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
}

trait SetAdjunction[R[_], L[_]] extends Adjunction[Scal, Function, Scal, Function, R, L] {
  final val leftCategory = summon
  final val rightCategory = summon

  def leftAdj[A, B](a: A)(f: L[A] => B): R[B]
  def rightAdj[A, B](la: L[A])(f: A => R[B]): B

  final def leftAdjunct[A: Scal, B: Scal](f: L[A] => B): A => R[B] =
    a => leftAdj(a)(f)

  final def rightAdjunct[A: Scal, B: Scal](f: A => R[B]): L[A] => B =
    la => rightAdj(la)(f)
}

object Adjunction extends AdjunctionInstances
