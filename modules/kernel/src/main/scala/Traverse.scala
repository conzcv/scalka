package scalka.kernel

import scalka.kernel.types.Kleisli

trait Traverse[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K], L[A <: K] <: K, F[A <: K] <: K]
  extends Endofunctor[K, Ob, [A <: K, B <: K] =>> Kleisli[K, Ob, Arr, F, A, B], L] {
    def traverse[A <: K, B <: K](ob: Ob[B])(f: Morphism[K, Ob, Arr, A, F[B]]): Morphism[K, Ob, Arr, L[A], F[L[B]]] =
      fmap(Morphism(f.domain, f, ob)).arrow
  }