import scalka.interop.cats.forgetScalkaFunctorK
import scalka.interop.cats.given
import scalka.kernel.{ScalMonad, ScalEndofunctor, ScalTraverse}
import scalka.kernel.types._
import scalka.syntax.functionK.function2K
import scalka.kernel.FunctionK
import scalka.syntax.functor.scal.*

class CatsSuite extends munit.FunSuite {
  test("cats interop suite") {
    summon[ScalEndofunctor[Function[Int, _]]].yoneda
    summon[ScalMonad[List]]
    summon[KleisliCat[Any, Scal, Function, List]]
    summon[ScalTraverse[List, Option]]


    val headOpt: FunctionK[Any, List, Option] =
       function2K[List, Option](_.headOption)

    val catsFunctionK = forgetScalkaFunctorK.fmap(headOpt)
    val result = catsFunctionK(List(1678,2,3,4,5,6))

    assert(result == Option(1678))
  }
}

