package scalka.syntax

import scalka.kernel.Representable

package object representable {
  extension [F[_], A, R] (fa: F[A]) {
    def index[Ob[_], ->[_, _]](using R: Representable.Aux[Ob, ->, F, R], ob: Ob[A]): R -> A = R.index(fa)
  }

  extension [->[_, _], R, A](f: R -> A) {
    def tabulate[Ob[_], F[_]](using R: Representable.Aux[Ob, ->, F, R], ob: Ob[A]): F[A] = R.tabulate(f)
  }
}
