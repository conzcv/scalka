package example.monoid

import cats.Monoid
import scalka.kernel.Applicable
import scalka.kernel.Category
import scalka.kernel.Free
import scalka.kernel.Functor
import scalka.kernel.types.Scal
import cats.syntax.foldable._

case class MonoidHom[A, B](toFun: A => B) {
  def apply(a: A): B = toFun(a)
}

given Applicable[Monoid, MonoidHom] = new Applicable[Monoid, MonoidHom] {
  def apply[A: Monoid, B: Monoid](a: A, f: MonoidHom[A, B]): B = f(a)
}

given Category[Monoid, MonoidHom] = new Category[Monoid, MonoidHom] {
  def compose[A: Monoid, B: Monoid, C: Monoid](f: MonoidHom[B, C], g: MonoidHom[A, B]): MonoidHom[A, C] = ???
  def id[A: Monoid]: MonoidHom[A, A] = MonoidHom(identity)
}

given Functor[Scal, Function, Monoid, MonoidHom, List] = new Functor[Scal, Function, Monoid, MonoidHom, List] {
  def fmap[A: Scal, B: Scal](f: A => B): MonoidHom[List[A], List[B]] = MonoidHom(_.map(f))
  def apply[A: Scal]: Monoid[List[A]] = summon
}

given Free[Monoid, MonoidHom, List] = new Free[Monoid, MonoidHom, List] {
  val left = summon
  val leftCategory: Category[Monoid, MonoidHom] = summon
  val applicable = summon

  def leftAdjunct[A: Scal, B: Monoid](f: MonoidHom[List[A], B]): A => B = a => f(List(a))
  def rightAdjunct[A: Scal, B: Monoid](f: A => B): MonoidHom[List[A], B] = MonoidHom(_.foldMap(f))
}
