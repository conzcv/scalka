package scalka.kernel

import scalka.kernel.types.IdK

trait Comonad[
  K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
  F[A <: K] <: K
] extends CoflatMap[K, Ob, ->, F] {
  def extract: Transform[F, IdK[K]]
}