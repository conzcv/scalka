package example.pointed

final class PointHom[-A, +B] private (val toFunction: A => B) extends AnyVal {
  final def andThen[C](f: PointHom[B, C]): PointHom[A, C] =
    new PointHom[A, C](toFunction andThen f.toFunction)
}

object PointHom {
  def unsafeLift[A, B](f: A => B): PointHom[A, B] =
    new PointHom[A, B](f)

  def lift[A: Point, B: Point](f: A => B): Option[PointHom[A, B]] =
    Option.when(Point[B].point == f(Point[A].point))(unsafeLift(f))

  def identity[A: Point]: PointHom[A, A] = unsafeLift(Predef.identity)
}
