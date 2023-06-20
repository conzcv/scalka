package scalka.kernel

import scalka.kernel.types._

trait Comonad[Ob[_], ->[_, _], F[_]] extends Extract[Ob, ->, F] with Duplicate[Ob, ->, F] {
  override def apply[A: Ob]: Ob[F[A]] = extract.domain[A]
}

object Comonad {
  given [Ob[_], ->[_, _], F[_]](using F: Free[Ob, ->, F]): Comonad[Ob, ->, F] =
    fromAdjunction(F)

  given [Ob[_], ->[_, _], F[_]](using C: Cofree[Ob, ->, F]): Comonad[Scal, Function, F] =
    fromAdjunction(C)

  def fromAdjunction[SOb[_], ->[_, _], DOb[_], ~>[_, _], R[_], L[_]](a: Adjunction[SOb, ->, DOb, ~>, R, L]): Comonad[SOb, ->, L o R]  =
    new Comonad[SOb, ->, L o R] { self =>
      val category = a.leftCategory
      val extract: Transform[L o R, Id] = a.counit
      val duplicate: Transform[L o R, L o R o L o R] =
        new Transform[L o R, L o R o L o R] {
          def domain[A: SOb]: SOb[(L o R)[A]] = self[A]

          def relation[A: SOb]: (L o R)[A] -> (L o R o L o R)[A] =
            given ra: DOb[R[A]] = a.right[A]
            given lra: SOb[(L o R)[A]] = self[A]
            given rlra: DOb[(R o L o R)[A]] = a.right[(L o R)[A]]
            a.left.fmap(a.leftAdjunct(fmap(a.leftCategory.id[A])))

          def codomain[A: SOb]: SOb[(L o R o L o R)[A]] =
            given lra: SOb[(L o R)[A]] = self[A]
            self[(L o R)[A]]
        }

      def fmap[A: SOb, B: SOb](f: A -> B): L[R[A]] -> L[R[B]] =
        given ra: DOb[R[A]] = a.right[A]
        given rb: DOb[R[B]] = a.right[B]
        a.left.fmap(a.right.fmap(f))
    }
}