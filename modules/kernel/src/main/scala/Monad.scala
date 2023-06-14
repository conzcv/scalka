package scalka.kernel

import scalka.kernel.types._

trait Monad[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends FlatMap[K, Ob, Arr, F] {
  type Pure[A <: K] = From[IdK[K], A]
  val pure: Transform[IdK[K], F]
}
