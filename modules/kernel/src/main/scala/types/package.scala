package scalka.kernel

package object types {
  sealed trait Scal[A]
  object Scal:
    private val impl = new Scal[Any] {}
    given [A]: Scal[A] = impl.asInstanceOf[Scal[A]]

  sealed trait ScalK[K <: AnyKind, F[A <: K]]
  object ScalK:
    private val impl = new ScalK[AnyKind, AnyK[AnyKind]] {}
    given [K <: AnyKind, F[A <: K]]: ScalK[K, F] = impl.asInstanceOf[ScalK[K, F]]

  type Id[+A] = A
  type ScalCategory1K[Arr[_, _]] = Category[Any, Scal, Arr]
  type ScalEndofunctor1K[F[_]] = Endofunctor[Any, Scal, Function, F]
  type ScalMonad1K[F[_]] = Monad[Any, Scal, Function, F]
  type ScalKleisli1K[F[_], A, B] = Kleisli[Any, Function, F, A, B]
  type ScalTraverse1K[L[_], G[_]] = Traverse[Any, Scal, Function, L, G]
  type ScalAdjunction1K[R[_], L[_]] = Adjunction[Any, Scal, Function, Any, Scal, Function, R, L]


  type IdK[K <: AnyKind] = [A <: K] =>> A
  type AnyK[K <: AnyKind] = [A <: K] =>> Any
  type ScalKCons[K <: AnyKind] = [F[A <: K]] =>> ScalK[K, F]
  type FunctionKCons[K <: AnyKind] = [F[A <: K], G[B <: K]] =>> FunctionK[K, F, G]
  type CatK[K <: AnyKind] = Category[AnyK[K], ScalKCons[K], FunctionKCons[K]]


  type Any2K[A] = AnyK[Any][A]
  type Scal2K[F[_]] = ScalK[Any, F]
  type Function2K[F[_], G[_]] = FunctionK[Any, F, G]
  type ScalCategory2K[F[_[_], _[_]]] = Category[Any2K, Scal2K, F]
  type ScalEndofunctor2K[F[G[_]] <: [A] =>> Any] = Endofunctor[Any2K, Scal2K, Function2K, F]
  type ScalMonad2K[F[G[_]] <: [A] =>> Any] = Monad[Any2K, Scal2K, Function2K, F]
  type ScalKleisli2K[F[G[_]] <: [A] =>> Any, A[_], B[_]] = Kleisli[Any2K, Function2K, F, A, B]


  type Kleisli[K <: AnyKind, ->[A <: K, B <: K], F[A <: K] <: K, A <: K, B <: K] = A -> F[B]

  type FunctorCat[
    SKind <: AnyKind, SOb[A <: SKind], SArr[A <: SKind, B <: SKind],
    DKind <: AnyKind, DOb[A <: DKind], DArr[A <: DKind, B <: DKind]
  ] = Category[
    [A <: SKind] =>> DKind,
    [F[A <: SKind] <: DKind] =>> Functor[SKind, SOb, SArr, DKind, DOb, DArr, F],
    [F[A <: SKind] <: DKind, G[A <: SKind] <: DKind] =>> Nat[SKind, SOb, DKind, DOb, DArr, F, G]
  ]

  type KleisliCat[
    K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K],
    F[A <: K] <: K
  ] = Category[K, Ob, [A <: K, B <: K] =>> Kleisli[K, ->, F, A, B]]   
}
