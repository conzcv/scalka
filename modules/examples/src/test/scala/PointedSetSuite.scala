import example.pointed.PointHom
import example.pointed.Point
import scalka.kernel.Applicable

import scalka.syntax.representable.*
import scalka.syntax.free.*
import scalka.kernel.types.Id
import example.pointed.given
import scalka.kernel.Comonad


class PointedSetSuite extends munit.FunSuite {
  type -->[-A, +B] = PointHom[A, B]

  extension [A: Point, B: Point](f: A --> B) {
    def apply(a: A)(using A: Applicable[Point, -->]) = A.apply(a, f)
  }

  given Point[Char] = new Point[Char] {
    def point: Char = '*'
  }

  given Point[String] = new Point[String] {
    def point: String = "point"
  }

  given Point[Byte] = new Point[Byte] {
    def point: Byte = 0.toByte
  }

  val one: Id[Char] = '1'
  val indexed: Option[Unit] --> Char = one.index[Point, PointHom]

  test("index") {
    val some = Option(())
    val none: Option[Unit] = None
    assert(indexed(some) == '1')
    assert(indexed(none) == '*')
  }

  test("comonad") {
    val exract = summon[Comonad[Point, PointHom, Option]].extract.relation[String]
    assert(exract(Option("kek")) == "kek")
    assert(exract(Option.empty[String]) == "point")
  }

  test("foldMap") {
    val some: Option[Char] = Some('$')
    val none: Option[Char] = None
    assert(some.foldMap[Point, -->](_.toByte) == '$'.toByte)
    assert(none.foldMap[Point, -->](_.toByte) == 0.toByte)
  }
  
}
