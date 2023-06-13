import scalka.kernel.{Endofunctor, Monad}
import scalka.kernel.types._
import scalka.interop.cats.given

class CatsSuite extends munit.FunSuite {
  test("summon") {
    summon[Endofunctor[Any, Scal, Function, Function[Int, _]]]
    summon[Monad[Any, Scal, Function, List]]
  }
}

