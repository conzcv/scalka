package scalka.kernel

import scalka.kernel.types.Scal

trait Representable[K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], F[A <: K]] {
  type Representation <: K
  val functor: Functor[K, Ob, Rel, Any, Scal, Function, F]
  type ->[A <: K, B <: K] = Morphism[K, Ob, Rel, A, B]
  def index[A <: K: Ob](fa: F[A]): Representation -> A
  def tabulate[A <: K](f: Representation -> A): F[A]
}