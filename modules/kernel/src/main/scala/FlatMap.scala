package scalka.kernel

trait FlatMap[
  K <: AnyKind,
  Ob[A <: K],
  Rel[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, Rel, F] {
  type Flatten[A <: K] = From[F o F, A]
  
  given category: Category[K, Ob, Rel]

  val flatten: Transform[F o F, F]

  final def flatMap[A <: K, B <: K: Ob](f: A -> F[B]): F[A] -> F[B] =
    fmap(f) >> flatten[B]
}