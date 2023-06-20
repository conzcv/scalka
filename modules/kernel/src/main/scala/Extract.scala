package scalka.kernel

import scalka.kernel.types.Id

trait Extract[Ob[_], ->[_, _], F[_]] extends Endofunctor[Ob, ->, F] {
    def extract: Transform[F, Id]
    def apply[A: Ob]: Ob[F[A]] = extract.domain[A]
}