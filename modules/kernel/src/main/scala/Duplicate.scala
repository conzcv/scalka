package scalka.kernel

import scalka.kernel.types.o

trait Duplicate[Ob[_], ->[_, _], F[_]] extends Endofunctor[Ob, ->, F]  {
  def duplicate: Transform[F, F o F]
  def apply[A: Ob]: Ob[F[A]] = duplicate.domain[A]

  def extend[A: Ob, B: Ob](f: F[A] -> B): F[A] -> F[B] =
    given fa: Ob[F[A]] = duplicate.domain[A]
    given fb: Ob[F[B]] = duplicate.domain[B]
    given ffa: Ob[F[F[A]]] = duplicate.codomain[A]
    category.compose(fmap(f), duplicate.relation[A])
}
