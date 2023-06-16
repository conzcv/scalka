package scalka.kernel

import scalka.kernel.types.Scal

trait Representable[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]] {
  type Representation <: K
  val rep: Ob[Representation]
  val functor: Functor[K, Ob, ->, Any, Scal, Function, F]
  def index[A <: K: Ob](fa: F[A]): Representation -> A
  def tabulate[A <: K: Ob](f: Representation -> A): F[A]
}