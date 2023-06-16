package scalka.kernel

import scalka.syntax.category.* 

trait FlatMap[
  K <: AnyKind,
  Ob[A <: K],
  ->[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, ->, F] {
  type Flatten[A <: K] = From[F o F, A]
  
  given category: Category[K, Ob, ->]

  val flatten: Transform[F o F, F]

  final def flatMap[A <: K: Ob, B <: K: Ob](f: A -> F[B]): F[A] -> F[B] =
    given fb: Ob[F[B]] = apply[B]
    given fa: Ob[F[A]] = apply[A]
    given ffb: Ob[F[F[B]]] = apply[F[B]]
    fmap(f) >>> flatten[B]
}