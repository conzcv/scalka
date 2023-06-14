import scalka.kernel.{Endofunctor, Monad}
import scalka.kernel.types._
import scalka.interop.cats.given
import scalka.interop.cats.forgetScalka
import scalka.kernel.Morphism
import scalka.syntax.functionK.functionK


class CatsSuite extends munit.FunSuite {
  test("cats interop suite") {
    summon[ScalEndofunctor1K[Function[Int, _]]]
    summon[ScalMonad1K[List]]

    val morphism: List --> Option =
       val funK = functionK[Any, List, Option](_.headOption)
       Morphism.fromArrow(funK)

    val result = forgetScalka.fmap(morphism).arrow(List(1678,2,3,4,5,6))

    assert(result == Option(1678))
  }
}

