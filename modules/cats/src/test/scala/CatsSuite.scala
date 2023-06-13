import scalka.kernel.Endofunctor
import scalka.kernel.types._
import scalka.interop.cats.given

class CatsSuite extends munit.FunSuite {
  test("summon") {
    summon[Endofunctor[Any, Scal, Function, List]]
  }
}

