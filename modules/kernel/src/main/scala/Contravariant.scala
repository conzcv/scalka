package scalka.kernel

import scalka.kernel.types.Scal

trait Contravariant[SOb[_], ->[_, _], DOb[_], ~>[_, _], F[_]] extends Functor[SOb, [A, B] =>> Op[->, A, B], DOb, ~>, F] {
  def contramap[A: SOb, B: SOb](f: A -> B): F[B] ~> F[A]

  def fmap[A: SOb, B: SOb](f: Op[->, A, B]): F[A] ~> F[B] =
    contramap(f.opposite)
}

trait ScalContravariant[Ob[_], ->[_, _], F[_]]
  extends ScalFunctor[Ob, [A, B] =>> Op[->, A, B], F] with Contravariant[Ob, ->, Scal, Function, F] {
    def on[A: Ob, B: Ob](fb: F[B])(f: A -> B): F[A]
    def map[A: Ob, B: Ob](fa: F[A])(f: Op[->, A, B]): F[B] =
      on(fa)(f.opposite)

    override def fmap[A: Ob, B: Ob](f: Op[->, A, B]): F[A] => F[B] =
      on(_)(f.opposite)

    def contramap[A: Ob, B: Ob](f: A -> B): F[B] => F[A] =
      fb => on(fb)(f)
  }