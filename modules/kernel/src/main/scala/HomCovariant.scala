package scalka.kernel

import scalka.syntax.category.>>>

sealed trait HomCovariant[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  R <: K
] extends ScalFunctor[K, Ob, ->, [B <: K] =>> R -> B]

object HomCovariant {
  def apply[
    K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
    R <: K: Ob
  ](using  C: Category[K, Ob, ->]): HomCovariant[K, Ob, ->, R] = new HomCovariant[K, Ob, ->, R] {
    def map[A <: K: Ob, B <: K: Ob](ra: R -> A)(f: A -> B): R -> B = ra >>> f
  }
}