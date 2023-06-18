package scalka.syntax.functor

import scalka.kernel.{ScalFunctor, Category, Yoneda}

package object scal {
  extension [Ob[_], ->[_, _], F[_]] (f: ScalFunctor[Ob, ->, F]) {
    def yoneda(using C: Category[Ob, ->]): Yoneda[Ob, ->, F] = Yoneda.forFunctor(f)
  }
}
