package scalka.kernel

import scalka.syntax.category.* 

trait Extend[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, ->, F] {

  given category: Category[K, Ob, ->]

  def duplicate: Transform[F, F o F]

  def apply[A <: K: Ob]: Ob[F[A]] = duplicate.domain[A]

  def extend[A <: K: Ob, B <: K: Ob](f: F[A] -> B): F[A] -> F[B] =
    given fa: Ob[F[A]] = duplicate.domain[A]
    given fb: Ob[F[B]] = duplicate.domain[B]
    given ffa: Ob[F[F[A]]] = duplicate.codomain[A]
    duplicate.relation[A] >>> fmap(f)
}