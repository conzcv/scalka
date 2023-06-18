package scalka.kernel

import scalka.syntax.category.>>>
import scalka.kernel.types.o

trait Bind[Ob[_], ->[_, _], F[_]] extends Endofunctor[Ob, ->, F] {
  
  given category: Category[Ob, ->]

  def flatten: Transform[F o F, F]

  def apply[A: Ob]: Ob[F[A]] = flatten.codomain[A]


  def bind[A: Ob, B: Ob](f: A -> F[B]): F[A] -> F[B] =
    given fb: Ob[F[B]] = flatten.codomain[B]
    given fa: Ob[F[A]] = flatten.codomain[A]
    given ffb: Ob[F[F[B]]] = flatten.domain[B]
    fmap(f) >>> flatten.relation[B]
}