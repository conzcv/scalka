package scalka.kernel

import scalka.syntax.category.>>>

sealed trait HomContravariant[K <: AnyKind, Ob[A <: K], ->[A <: K, B  <: K], R <: K]
  extends ScalContravariant[K, Ob, ->, [A <: K] =>> A -> R]

object HomContravariant {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], R <: K: Ob](using Category[K, Ob, ->]): HomContravariant[K, Ob, ->, R] =
    new HomContravariant[K, Ob, ->, R] {
      def on[A <: K: Ob, B <: K: Ob](br: B -> R)(f: A -> B): A -> R =  f >>> br
    }
}