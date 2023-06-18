package scalka.syntax
import scalka.kernel.Category

package object category {
  extension [K <: AnyKind, ->[A <: K, B <: K], A <: K, B <: K](f: A -> B) {
    def >>>[Ob[A <: K], C <: K](g: B -> C)(using Category[K, Ob, ->], Ob[A], Ob[B], Ob[C]): A -> C =
      summon[Category[K, Ob, ->]].compose(g, f)
  }

  extension [Kind <: AnyKind, Ob[A <: Kind], A <: Kind](obj: Ob[A]) {
    def id[->[A <: Kind, B <: Kind]](using C: Category[Kind, Ob, ->]) = C.id(obj)
  }
}
