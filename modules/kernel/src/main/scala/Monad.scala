package scalka.kernel

import scalka.kernel.types._

trait Monad[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, Arr, F] {

  type From[G[A <: K] <: K, A <: K] = Morphism[K, Ob, Arr, G[A], F[A]]
  type Pure[A <: K] = From[IdK[K], A]
  type Flatten[A <: K] = From[F o F, A]
  
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = EndoNat[K, Ob, Arr, F, G]
  type o[F[A <: K] <: K, G[A <: K] <: K] = [A <: K] =>> F[F[A]]

  given category: Cat[K, Ob, Arr]

  val pure: Transform[IdK[K], F]

  val flatten: Transform[F o F, F]
  
  def fmap[A <: K, B <: K](f: A -> B): F[A] -> F[B]

  final def flatMap[A <: K, B <: K](ob: Ob[B])(f: A -> F[B]): F[A] -> F[B] =
    fmap(f) >> flatten(ob)
}
