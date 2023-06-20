package syntax

import scalka.kernel.Monad

package object monad {
  def pure[Ob[_], ->[_, _], F[_], A: Ob](using M: Monad[Ob, ->, F]): A -> F[A] = M.pure.relation[A]
}
