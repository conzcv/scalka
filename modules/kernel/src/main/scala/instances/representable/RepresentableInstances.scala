package scalka.instances.representable

import scalka.kernel.{Representable, Free}
import scalka.kernel.types.Id

trait RepresentableInstances {
    given [Ob[_], ->[_, _], F[_]](using F: Free[Ob, ->, F]): Representable.Aux[Ob, ->, Id, F[Unit]] =
      new Representable[Ob, ->, Id] {
        type Representation = F[Unit]
        val functor = F.right
        val representation = F.left[Unit]
        def index[A: Ob](fa: A): F[Unit] -> A = F.rightAdjunct(_ => fa)
        def tabulate[A: Ob](f: F[Unit] -> A): A = F.leftAdjunct(f).apply(())
    }
}
