package scalka.kernel

import scalka.syntax.category.* 

trait Bind[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, ->, F] {
  
  given category: Category[K, Ob, ->]

  def flatten: Transform[F o F, F]

  def apply[A <: K: Ob]: Ob[F[A]] = flatten.codomain[A]


  def bind[A <: K: Ob, B <: K: Ob](f: A -> F[B]): F[A] -> F[B] =
    given fb: Ob[F[B]] = flatten.codomain[B]
    given fa: Ob[F[A]] = flatten.codomain[A]
    given ffb: Ob[F[F[B]]] = flatten.domain[B]
    fmap(f) >>> flatten.relation[B]
}