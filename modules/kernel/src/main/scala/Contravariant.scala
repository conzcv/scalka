package scalka.kernel

import scalka.kernel.types.Scal

trait Contravariant[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B  <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D
] extends Functor[S, SOb, [A <: S, B <: S] =>> Op[S, ->, A, B], D, DOb, ~>, F] {
  def contramap[A <: S: SOb, B <: S: SOb](f: A -> B): F[B] ~> F[A]

  final def fmap[A <: S: SOb, B <: S: SOb](f: Op[S, ->, A, B]): F[A] ~> F[B] =
    contramap(f.opposite)
}

trait ScalContravariant[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]]
  extends Contravariant[K, Ob, ->, Any, Scal, Function, F] {
    def on[A <: K: Ob, B <: K: Ob](fb: F[B])(f: A -> B): F[A]

    final def apply[A <: K: Ob]: Scal[F[A]] = summon

    final def contramap[A <: K: Ob, B <: K: Ob](f: A -> B): F[B] => F[A] =
      fb => on(fb)(f)
  }