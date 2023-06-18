package scalka.kernel

import scalka.syntax.category.>>>

sealed trait HomCovariant[Ob[_], ->[_, _], R] extends ScalFunctor[Ob, ->, [B] =>> R -> B]

object HomCovariant {
  def apply[Ob[_], ->[_, B], R: Ob](using  C: Category[Ob, ->]): HomCovariant[Ob, ->, R] = new HomCovariant[Ob, ->, R] {
    def map[A: Ob, B: Ob](ra: R -> A)(f: A -> B): R -> B = ra >>> f
  }
}