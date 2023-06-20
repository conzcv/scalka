package scalka.kernel

import scalka.kernel.types._

trait Cofree[Ob[_], ->[_, _], R[_]] extends Adjunction[Scal, Function, Ob, ->, R, Id] {
  val applicable: Applicable[Ob, ->]
  final val leftCategory = summon

  final def left: ScalForgetful[Ob, ->] = ScalForgetful(applicable)

  final def unfoldMap[A: Ob, B](a: A)(f: A => B): R[B] =
    given Ob[R[B]] = right[B]
    applicable(a, leftAdjunct(f))

  final def unfold[A: Ob](la: A): R[A] =
    given Ob[R[A]] = right[A]
    applicable(la, unit.relation[A])
  
  final def instance[A]: Ob[R[A]] = right[A]

  final def extract[A](ra: R[A]): A =
    counit.relation[A].apply(ra)
}