import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.cat._

class KernelSuite extends munit.FunSuite {

  test("extensions and summon") {
    summon[Scal[String]].id[<:<]
    val function: Int => String = (a: Int) => a.toString()
    val morphism = function.toMorphism[Scal](summon, summon)
    val identity = summon[Scal[Int]].id[Function]
    val functorCat = summon[FunctorCat[Any, Scal, <:<, Any, Scal, Function]]
    assert(identity.arrow(345) == 345)
  }
}

