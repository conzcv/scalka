import scalka.interop.cats.given
import scalka.kernel.{ScalMonad, ScalEndofunctor, ScalTraverse}
import scalka.kernel.types._
import scalka.syntax.functor.scal.*

class CatsSuite extends munit.FunSuite {
  test("cats interop suite") {
    summon[ScalEndofunctor[Function[Int, _]]].yoneda
    summon[ScalMonad[List]]
    summon[KleisliCat[Scal, Function, List]]
    summon[ScalTraverse[List, Option]]
  }
}

