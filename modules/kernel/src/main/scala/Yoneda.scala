package scalka.kernel

type Cov[K <: AnyKind, ->[A <: K, B <: K], A <: K] = [X <: K] =>> A -> X

sealed trait Yoneda[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]] {
  def delay[A <: K: Ob](fa: F[A]): ScalNat[K, Ob, Cov[K, ->, A], F]
  def run[A <: K: Ob](nat: ScalNat[K, Ob, Cov[K, ->, A], F]): F[A]
}

object Yoneda {
  def forFunctor[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]](
    functor: ScalFunctor[K, Ob, ->, F]
  )(using category: Category[K, Ob, ->]): Yoneda[K, Ob, ->, F] = new Yoneda[K, Ob, ->, F] {
    def delay[A <: K: Ob](fa: F[A]): ScalNat[K, Ob, Cov[K, ->, A], F] =
      new ScalNat[K, Ob, Cov[K, ->, A], F] {
        def run[X <: K: Ob](ax: A -> X): F[X] =
          functor.map(fa)(ax)
      }

    def run[A <: K: Ob](nat: ScalNat[K, Ob, Cov[K, ->, A], F]): F[A] =
      nat.run(category.id[A])
  }
}
