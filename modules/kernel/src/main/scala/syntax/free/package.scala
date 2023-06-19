package scalka.syntax

import scalka.kernel.Free

package object free {
  extension [F[_], A, B](fa: F[A]) {
    def foldMap[Ob[_], ->[_, _]](f: A => B)(using b: Ob[B], free: Free[Ob, ->, F]): B =
      free.foldMap(fa)(f)
  }
}
