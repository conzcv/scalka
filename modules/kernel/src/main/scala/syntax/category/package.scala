package scalka.syntax
import scalka.kernel.Category

package object category {
  extension [K <: AnyKind, ->[A <: K, B <: K], A <: K, B <: K](f: A -> B) {
    def >>>[Ob[A <: K], C <: K](g: B -> C)(using Category[K, Ob, ->], Ob[A], Ob[B], Ob[C]): A -> C =
      summon[Category[K, Ob, ->]].compose(g, f)
  }
}
