package scalka.kernel

import scalka.kernel.types.IdK

trait Comonad[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends CoflatMap[K, Ob, Arr, F] {
  type Extract[A <: K] = To[IdK[K], A]
  val extract: Transform[F, IdK[K]]
}