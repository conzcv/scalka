package scalka.kernel

import scalka.kernel.types._

trait Free[Ob[_], ->[_, _], L[_]] extends Adjunction[Ob, ->, Scal, Function, Id, L] {
  given applicable: Applicable[Ob, ->]
  final val right: ScalForgetful[Ob, ->] = summon
  final val rightCategory: Category[Scal, Function] = summon

  final def foldMap[A, B: Ob](fa: L[A])(f: A => B): B =
    given Ob[L[A]] = left[A]
    applicable(fa, rightAdjunct(f))
  
  final def instance[A]: Ob[L[A]] = left[A]

  final def extract[A: Ob]: L[A] -> A =
    comonad.extract.relation[A]

  final def pure[A](a: A): L[A] =
    monad.pure.relation[A].apply(a)

}