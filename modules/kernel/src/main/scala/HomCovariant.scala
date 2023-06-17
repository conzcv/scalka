package scalka.kernel

import scalka.kernel.types.Scal
import scalka.syntax.category.>>>

sealed trait HomCovariant[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  R <: K
] extends Functor[K, Ob, ->, Any, Scal, Function, [B <: K] =>> R -> B]

object HomCovariant {
  def apply[
    K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
    R <: K: Ob
  ](using  C: Category[K, Ob, ->]): HomCovariant[K, Ob, ->, R] = new HomCovariant[K, Ob, ->, R] {
    def fmap[A <: K: Ob, B <: K: Ob](f: A -> B): (R -> A) => (R -> B) =
      ra => ra >>> f
      
    def apply[A <: K: Ob]: Scal[R -> A] =
      summon
  }
}