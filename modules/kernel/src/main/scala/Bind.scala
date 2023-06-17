package scalka.kernel

import scalka.syntax.category.* 

trait Bind[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, ->, F] {
  
  given category: Category[K, Ob, ->]

  def flatten: Transform[F o F, F]

  def bind[A <: K: Ob, B <: K: Ob](f: A -> F[B]): F[A] -> F[B] =
    given fb: Ob[F[B]] = apply[B]
    given fa: Ob[F[A]] = apply[A]
    given ffb: Ob[F[F[B]]] = apply[F[B]]
    fmap(f) >>> flatten.relation[B]
}