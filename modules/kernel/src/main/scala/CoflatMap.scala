package scalka.kernel

import scalka.syntax.category.* 

trait CoflatMap[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, ->, F] {

  given category: Category[K, Ob, ->]

  def duplicate: Transform[F, F o F]

  def extend[A <: K: Ob, B <: K: Ob](f: F[A] -> B): F[A] -> F[B] =
    given fa: Ob[F[A]] = apply[A]
    given fb: Ob[F[B]] = apply[B]
    given ffa: Ob[F[F[A]]] = apply[F[A]]
    duplicate.relation[A] >>> fmap(f)
}