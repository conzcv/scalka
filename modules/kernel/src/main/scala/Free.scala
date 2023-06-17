package scalka.kernel

import scalka.kernel.types._

trait Free[Ob[_], ->[_, _], L[_]] extends Adjunction[Any, Ob, ->, Any, Scal, Function, Id, L] {
  given applicable: Applicable[Ob, ->]
  val right = summon
  val rightCategory = summon

  final def foldMap[A, B: Ob](fa: L[A])(f: A => B): B =
    given Ob[L[A]] = left[A]
    applicable.toFunction(rightAdjunct(f)).apply(fa)
  
  final def construct[A]: Ob[L[A]] = left[A]

}