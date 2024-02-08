package example.pointed

import scalka.kernel.Applicable
import scalka.kernel.Category
import scalka.kernel.Functor
import scalka.kernel.types.Scal
import scalka.kernel.Free

given Applicable[Point, PointHom] = new Applicable[Point, PointHom] {
  def apply[A: Point, B: Point](a: A, f: PointHom[A, B]): B = f.toFunction(a)
}

given Category[Point, PointHom] = new Category[Point, PointHom] {
  def compose[A: Point, B: Point, C: Point](f: PointHom[B, C], g: PointHom[A, B]): PointHom[A, C] =
    g andThen f
  def id[A: Point]: PointHom[A, A] =
    PointHom.identity
}

given Functor[Scal, Function, Point, PointHom, Option] = new Functor[Scal, Function, Point, PointHom, Option] {
  def apply[A: Scal]: Point[Option[A]] = summon
  def fmap[A: Scal, B: Scal](f: A => B): PointHom[Option[A], Option[B]] =
    PointHom.unsafeLift(_.map(f))
}

given Free[Point, PointHom, Option] = new  Free[Point, PointHom, Option] {
  val left = summon
  val leftCategory = summon
  val applicable: Applicable[Point, PointHom] = summon
  def leftAdjunct[A: Scal, B: Point](f: PointHom[Option[A], B]): A => B =
    (a: A) => applicable(Option(a), f)
  def rightAdjunct[A: Scal, B: Point](f: A => B): PointHom[Option[A], B] =
    PointHom.unsafeLift(_.fold(Point[B].point)(f))
}