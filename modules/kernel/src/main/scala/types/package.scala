package scalka.kernel

package object types {
  sealed trait Scal[A]
  object Scal:
    private val impl = new Scal[Any] {}
    given [A]: Scal[A] = impl.asInstanceOf[Scal[A]]

  type Id[+A] = A
  type o[F[_], G[_]] = [A] =>> F[G[A]]

  type Kleisli[->[_, _], F[_], A, B] = A -> F[B]
  type Cokleisli[->[_, _], F[_], A, B] = F[A] -> B 

  type Presheaf[Ob[_], ->[_, _], F[_]] =
    ScalContravariant[Ob, ->, F]

  type KleisliCat[Ob[_], ->[_, _], F[_]] = Category[Ob, Kleisli[->, F, _, _]]
  type CokleisliCat[Ob[_], ->[_ , _], F[_]] = Category[Ob, Cokleisli[->, F, _, _]]   
}
