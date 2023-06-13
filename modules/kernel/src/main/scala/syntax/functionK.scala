package scalka.syntax

import scalka.kernel.FunctionK

object functionK {

  abstract class Maker[K <: AnyKind, F[A <: K], G[A <: K], Arbitrary <: K] extends FunctionK[K, F, G] {

    def applyArbitrary(f: F[Arbitrary]): G[Arbitrary]

    def apply[A <: K](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[G[A]]
  }

  final class Applied[K <: AnyKind, F[A <: K], G[A <: K]](private val __ : Boolean) extends AnyVal {
    type Arbitrary <: K
    def apply(maker: Maker[K, F, G, Arbitrary]): FunctionK[K, F, G]  = maker
  }

  def functionK[K <: AnyKind, F[A <: K], G[A <: K]] = new Applied[K, F, G](true)
}
