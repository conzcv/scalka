package scalka.kernel

import scalka.kernel.types.Kleisli

trait Traverse[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], L[A <: K] <: K, F[A <: K] <: K]
  extends Endofunctor[K, Ob, [A <: K, B <: K] =>> Kleisli[K, ->, F, A, B], L] {
    def traverse[A <: K: Ob, B <: K: Ob](f: A -> F[B]): L[A] -> F[L[B]] = fmap(f)
  }