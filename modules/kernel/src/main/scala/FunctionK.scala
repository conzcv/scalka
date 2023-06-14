package scalka.kernel

trait FunctionK[K <: AnyKind, F[A <: K], G[A <: K]] { self =>
  def apply[A <: K](fa: F[A]): G[A]
  def andThen[H[A <: K]](f: FunctionK[K, G, H]): FunctionK[K, F, H] = new FunctionK[K, F, H] {
    def apply[A <: K](fa: F[A]): H[A] = f(self(fa))
  }
}

object FunctionK {
  def id[K <: AnyKind, F[A <: K]]: FunctionK[K, F, F] = new FunctionK[K, F, F] {
    def apply[A <: K](fa: F[A]): F[A] = fa
  }
}
