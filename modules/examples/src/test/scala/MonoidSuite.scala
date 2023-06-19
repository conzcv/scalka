
import example.monoid.MonoidHom
import example.monoid.given
import scalka.syntax.representable.*
import scalka.kernel.types.Id
import cats.kernel.Monoid

class MonoidSuite extends munit.FunSuite {
  test("example") {
    val one: Id[Int] = 345
    val fourUnit = () +: () +: () +: () +: Nil
    val indexed = one.index[Monoid, MonoidHom, List[Unit]]
    assert(indexed(fourUnit) == 4 * one)

    val tabulated = indexed.tabulate[Monoid, Id]
    assert(tabulated == one)
  }
}
