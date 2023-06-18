package scalka.kernel

import scalka.syntax.category.>>>

sealed trait HomContravariant[Ob[_], ->[_, _], R]
  extends ScalContravariant[Ob, ->, [A] =>> A -> R]

object HomContravariant {
  def apply[Ob[_], ->[_, _], R: Ob](using Category[Ob, ->]): HomContravariant[Ob, ->, R] =
    new HomContravariant[Ob, ->, R] {
      def on[A: Ob, B: Ob](br: B -> R)(f: A -> B): A -> R =  f >>> br
    }
}