package scalka.syntax.functor

import scalka.kernel.{ScalFunctor, Category, Yoneda}

package object scal {
  extension [K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]] (f: ScalFunctor[K, Ob, ->, F]) {
    def yoneda(using C: Category[K, Ob, ->]): Yoneda[K, Ob, ->, F] = Yoneda.forFunctor(f)
  }
}
