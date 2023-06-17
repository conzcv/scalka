package scalka.kernel

import scalka.kernel.types.Scal

trait Presheaf[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]]
  extends Contravariant[K, Ob, ->, Any, Scal, Function, F] {
    def on[A <: K: Ob, B <: K: Ob](fb: F[B])(f: A -> B): F[A]

    def contramap[A <: K: Ob, B <: K: Ob](f: A -> B): F[B] => F[A] =
      fb => on(fb)(f)
  }
