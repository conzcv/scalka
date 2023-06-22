package example.monoid

import cats.Monoid
import scalka.kernel.Applicable
import scalka.kernel.Category
import scalka.kernel.Free
import scalka.kernel.Functor
import scalka.kernel.types.Scal
import cats.syntax.foldable._
import cats.evidence.Is

sealed trait MonoidHom[-A, +B]
case class Compose[-A, B, +C](f: MonoidHom[B, C], g: MonoidHom[A, B], B: Monoid[B]) extends MonoidHom[A, C]
case class Identity[A]() extends MonoidHom[A, A]
case class ListHom[-A, +B](f: A => B) extends MonoidHom[List[A], List[B]]
case class ListFold[-A, +B](f: A => B) extends MonoidHom[List[A], B]
case class Isomorphism[A, B](underlying: A Is B) extends MonoidHom[A, B] {
  def flip: Isomorphism[B, A] = Isomorphism(underlying.flip)
}

given Applicable[Monoid, MonoidHom] = new Applicable[Monoid, MonoidHom] {
  def apply[A: Monoid, B: Monoid](a: A, f: MonoidHom[A, B]): B = f match
    case Compose(f, g, b) => apply(apply(a, g)(summon, b), f)(b, summon)
    case Identity() => a
    case ListHom(f) => a.map(f)
    case ListFold(f) => a.foldMap(f)
    case Isomorphism(iso) => iso.coerce(a)
}

given Category[Monoid, MonoidHom] = new Category[Monoid, MonoidHom] {
  def compose[A: Monoid, B: Monoid, C: Monoid](f: MonoidHom[B, C], g: MonoidHom[A, B]): MonoidHom[A, C] =
    Compose(f, g, summon)
  def id[A: Monoid]: MonoidHom[A, A] =
    Identity()
}

given Functor[Scal, Function, Monoid, MonoidHom, List] = new Functor[Scal, Function, Monoid, MonoidHom, List] {
  def fmap[A: Scal, B: Scal](f: A => B): MonoidHom[List[A], List[B]] = ListHom(f)
  def apply[A: Scal]: Monoid[List[A]] = summon
}

given Free[Monoid, MonoidHom, List] = new Free[Monoid, MonoidHom, List] {
  val left = summon
  val leftCategory: Category[Monoid, MonoidHom] = summon
  val applicable = summon

  def leftAdjunct[A: Scal, B: Monoid](f: MonoidHom[List[A], B]): A => B =
    a => applicable.apply(List(a), f)
  def rightAdjunct[A: Scal, B: Monoid](f: A => B): MonoidHom[List[A], B] =
    ListFold(f)
}
