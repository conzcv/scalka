package scalka.kernel

import scalka.instances.representable.RepresentableInstances

trait Representable[Ob[_], ->[_, _], F[_]] {
  type Representation
  val representation: Ob[Representation]
  val functor: ScalFunctor[Ob, ->, F]
  def index[A: Ob](fa: F[A]): Representation -> A
  def tabulate[A: Ob](f: Representation -> A): F[A]
}

object Representable extends RepresentableInstances {
  type Aux[Ob[_], ->[_, _], F[_], R] =
    Representable[Ob, ->, F] {
      type Representation = R
    }
}