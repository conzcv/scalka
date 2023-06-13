package scalka.kernel

trait FunctionK[K <: AnyKind, F[A <: K], G[A <: K]] {
  def apply[A <: K](fa: F[A]): G[A]
}
