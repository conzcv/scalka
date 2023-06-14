import scalka.kernel.{Endofunctor, Monad}
import scalka.kernel.types._
import scalka.interop.cats.given
import scalka.interop.cats.forgetScalkaFunctorK
import scalka.kernel.Morphism
import scalka.syntax.functionK.function2K
import scalka.kernel.Traverse


class CatsSuite extends munit.FunSuite {
  test("cats interop suite") {
    summon[ScalEndofunctor1K[Function[Int, _]]]
    summon[ScalMonad1K[List]]
    summon[KleisliCat[Any, Scal, Function, List]]
    summon[ScalTraverse1K[List, Option]]


    val morphism: List --> Option =
       val funK = function2K[List, Option](_.headOption)
       Morphism.fromArrow(funK)

    val result = forgetScalkaFunctorK.fmap(morphism).arrow(List(1678,2,3,4,5,6))

    assert(result == Option(1678))
  }
}

