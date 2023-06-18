package scalka.kernel

import scalka.instances.representable.RepresentableInstances

trait Representable[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]] {
  type Representation <: K
  val representation: Ob[Representation]
  val functor: ScalFunctor[K, Ob, ->, F]
  def index[A <: K: Ob](fa: F[A]): Representation -> A
  def tabulate[A <: K: Ob](f: Representation -> A): F[A]
}

object Representable extends RepresentableInstances {
  type Aux[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K], R <: K] =
    Representable[K, Ob, ->, F] {
      type Representation = R
    }
}