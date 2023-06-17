package scalka.kernel

trait ApplicableK[K <: AnyKind, Ob[F[A <: K]], ->[X[A <: K], Y[B <: K]]] {
  def toFunctionK[F[A <: K]: Ob, G[B <: K]: Ob](f: F -> G): FunctionK[K, F, G]
}