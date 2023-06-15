package scalka.kernel

import scalka.syntax.functionK.functionK

trait CoflatMap[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, Arr, F] {
  type Coflatten[A <: K] = To[F o F, A]

  given category: Cat[K, Ob, Arr]

  val coflatten: Transform[F, F o F]

  final def coflatMap[A <: K, B <: K](ob: Ob[A])(f: F[A] -> B): F[A] -> F[B] =
    coflatten(ob) >> fmap(f)
}