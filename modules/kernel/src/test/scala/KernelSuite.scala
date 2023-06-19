import scalka.kernel._
import scalka.kernel.types._
import scalka.syntax.category.id

class KernelSuite extends munit.FunSuite {
  test("extensions and summon") {
    summon[Scal[String]].id[<:<]
    val identity = summon[Scal[Int]].id[Function]
    assert(identity(345) == 345)
  }
}

