package example.pointed

trait Point[+A] {
  def point: A
}

object Point {
  given [A]: Point[Option[A]] = new Point[Option[A]] {
    def point: Option[A] = None
  }
  inline def apply[A](using P: Point[A]): Point[A] = P
}
