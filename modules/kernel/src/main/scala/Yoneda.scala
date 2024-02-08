package scalka.kernel

type Cov[->[_, _], A] = [X] =>> A -> X

sealed trait Yoneda[Ob[_], ->[_, _], F[_]] {
  def delay[A: Ob](fa: F[A]): ScalNat[Ob, Cov[->, A], F]
  def run[A: Ob](nat: ScalNat[Ob, Cov[->, A], F]): F[A]
}

object Yoneda {
  def forFunctor[Ob[_], ->[_, _], F[_]](
    functor: ScalFunctor[Ob, ->, F]
  )(using category: Category[Ob, ->]): Yoneda[Ob, ->, F] = new Yoneda[Ob, ->, F] {
    def delay[A: Ob](fa: F[A]): ScalNat[Ob, Cov[->, A], F] =
      new ScalNat[Ob, Cov[->, A], F] {
        def run[X: Ob](ax: A -> X): F[X] =
          functor.map(fa)(ax)
      }

    def run[A: Ob](nat: ScalNat[Ob, Cov[->, A], F]): F[A] =
      nat.run(category.id[A])
  }
}
