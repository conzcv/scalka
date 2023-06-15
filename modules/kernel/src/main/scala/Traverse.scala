package scalka.kernel

import scalka.kernel.types.Kleisli

trait Traverse[K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], L[A <: K] <: K, F[A <: K] <: K]
  extends Endofunctor[K, Ob, [A <: K, B <: K] =>> Kleisli[K, Ob, Rel, F, A, B], L] {
    def traverse[A <: K, B <: K: Ob](f: Morphism[K, Ob, Rel, A, F[B]]): Morphism[K, Ob, Rel, L[A], F[L[B]]] =
      fmap(Morphism(f.domain, f, summon)).arrow
  }