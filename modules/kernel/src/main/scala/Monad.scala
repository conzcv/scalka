package scalka.kernel

import scalka.kernel.types._

trait Monad[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, Arr, F] {
  type Transform[F[A <: K] <: K, G[A <: K] <: K] = EndoNat[K, Ob, Arr, F, G]
  type FF[A <: K] = F[F[A]]
  given category: Cat[K, Ob, Arr]
  val pure: Transform[IdK[K], F]
  final val flatten: Transform[FF, F] = new Transform[FF, F] {
    def apply[A <: K](ob: Ob[A]): FF[A] -> F[A] =
      flatMap(category.id(pure(ob).codomain))
  }
  def flatMap[A <: K, B <: K](f: A -> F[B]): F[A] -> F[B]
  
  final def fmap[A <: K, B <: K](f: A -> B): F[A] -> F[B] =
    flatMap(f >> pure(f.codomain))
    
}
