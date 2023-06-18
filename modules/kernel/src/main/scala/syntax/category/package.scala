package scalka.syntax
import scalka.kernel.Category

package object category {
  extension [->[_, _], A, B](f: A -> B) {
    def >>>[Ob[_], C](g: B -> C)(using Category[Ob, ->], Ob[A], Ob[B], Ob[C]): A -> C =
      summon[Category[Ob, ->]].compose(g, f)
  }

  extension [Ob[_], A](obj: Ob[A]) {
    def id[->[A, B]](using C: Category[Ob, ->]) = C.id(obj)
  }
}
