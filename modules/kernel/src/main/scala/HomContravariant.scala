package scalka.kernel

import scalka.kernel.types.Scal
import scalka.syntax.category.>>>

sealed trait HomContravariant[K <: AnyKind, Ob[A <: K], ->[A <: K, B  <: K], R <: K]
  extends Contravariant[K, Ob, ->, Any, Scal, Function, [A <: K] =>> A -> R]

object HomContravariant {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], R <: K: Ob](using Category[K, Ob, ->]): HomContravariant[K, Ob, ->, R] =
    new HomContravariant[K, Ob, ->, R] {
      def contramap[A <: K: Ob, B <: K: Ob](f: A -> B): B -> R => A -> R =
        br => f >>> br
      def apply[A <: K: Ob]: Scal[A -> R] = summon
    }
}