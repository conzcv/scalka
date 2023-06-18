package scalka.kernel

import scalka.kernel.types.Id

trait Comonad[Ob[_], ->[_, _], F[_]] extends Extend[Ob, ->, F] {
  def extract: Transform[F, Id]
}