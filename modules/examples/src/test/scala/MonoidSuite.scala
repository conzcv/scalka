
import example.monoid.MonoidHom
import example.monoid.given
import scalka.syntax.representable.*
import scalka.syntax.free.*
import scalka.kernel.Comonad
import scalka.kernel.types.Id
import cats.kernel.Monoid

class MonoidSuite extends munit.FunSuite {
  type ==>[A, B] = MonoidHom[A, B]
  val one: Id[Int] = 42
  val indexed: List[Unit] ==> Int = one.index[Monoid, ==>]

  test("index") {
    val fourUnit = () +: () +: () +: () +: Nil
    assert(indexed(fourUnit) == 4 * one)
  }

  test("tabulate") {
    val tabulated = indexed.tabulate[Monoid, Id]
    assert(tabulated == one)
  }

  test("comonad") {
    val exract = summon[Comonad[Monoid, MonoidHom, List]].extract.relation[String]
    assert(exract(List("Hello", " World", "!")) == "Hello World!")
  }

  test("foldMap") {
    val foldable: List[Char] = List('k', 'e', 'k')
    val result: Int = foldable.foldMap[Monoid, ==>](_.toInt)
    assert(result == foldable.foldLeft(0)((acc, c) => acc + c.toInt))
  }
}
