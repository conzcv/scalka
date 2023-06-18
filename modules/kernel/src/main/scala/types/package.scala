package scalka.kernel

package object types {
  sealed trait Scal[A]
  object Scal:
    private val impl = new Scal[Any] {}
    given [A]: Scal[A] = impl.asInstanceOf[Scal[A]]

  type Id[+A] = A
  type o[F[_], G[_]] = [A] =>> F[G[A]]

  type Kleisli[->[_, _], F[_], A, B] = A -> F[B]

  type Presheaf[Ob[_], ->[_, _], F[_]] =
    ScalContravariant[Ob, ->, F]

  type KleisliCat[Ob[A], ->[A, B], F[A]] = Category[Ob, [A, B] =>> Kleisli[->, F, A, B]]   
}
