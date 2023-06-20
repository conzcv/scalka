package scalka.kernel

import scalka.kernel.types._

trait Free[Ob[_], ->[_, _], L[_]] extends Adjunction[Ob, ->, Scal, Function, Id, L] {
  val applicable: Applicable[Ob, ->]
  final val rightCategory: Category[Scal, Function] = summon
  final def right: ScalForgetful[Ob, ->] = ScalForgetful(applicable)

  final def foldMap[A, B: Ob](fa: L[A])(f: A => B): B =
    given Ob[L[A]] = left[A]
    applicable(fa, rightAdjunct(f))

  final def fold[A: Ob](la: L[A]): A =
    given Ob[L[A]] = left[A]
    applicable(la, counit.relation[A])
  
  final def instance[A]: Ob[L[A]] = left[A]

  final def pure[A](a: A): L[A] =
    unit.relation[A].apply(a)

}