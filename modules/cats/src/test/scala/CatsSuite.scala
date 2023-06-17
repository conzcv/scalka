import scalka.interop.cats.forgetScalkaFunctorK
import scalka.interop.cats.given
import scalka.kernel.{SetMonad, SetEndofunctor, SetTraverse}
import scalka.kernel.types._
import scalka.syntax.functionK.function2K
import scalka.kernel.FunctionK

class CatsSuite extends munit.FunSuite {
  test("cats interop suite") {
    summon[SetEndofunctor[Function[Int, _]]]
    summon[SetMonad[List]]
    summon[KleisliCat[Any, Scal, Function, List]]
    summon[SetTraverse[List, Option]]


    val headOpt: FunctionK[Any, List, Option] =
       function2K[List, Option](_.headOption)

    val catsFunctionK = forgetScalkaFunctorK.fmap(headOpt)
    val result = catsFunctionK(List(1678,2,3,4,5,6))

    assert(result == Option(1678))
  }
}

