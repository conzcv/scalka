package scalka.kernel

import scalka.kernel.types._

trait Pure[Ob[_], ->[_, _], F[_]] extends Endofunctor[Ob, ->, F] {
  def pure: Transform[Id, F]
  def apply[A: Ob]: Ob[F[A]] = pure.codomain[A]
}
