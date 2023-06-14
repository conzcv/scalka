import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.cat._

class KernelSuite extends munit.FunSuite {

  test("extensions and summon") {
    summon[Scal[String]].id[<:<]
    val function: Int => String = (a: Int) => a.toString()
    val morphism: Int ==> String = Morphism.fromArrow(function)
    val identity = summon[Scal[Int]].id[Function]
    val functorCat = summon[FunctorCat[Any, Scal, <:<, Any, Scal, Function]]
    assert(identity.arrow(345) == 345)
  }
}

