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

  type IdK[K <: AnyKind] = [A <: K] =>> A
  type AnyK[K <: AnyKind] = [A <: K] =>> Any
  type ScalKCons[K <: AnyKind] = [F[A <: K]] =>> ScalK[K, F]
  type FunctionKCons[K <: AnyKind] = [F[A <: K], G[B <: K]] =>> FunctionK[K, F, G]
  type CatK[K <: AnyKind] = Category[AnyK[K], ScalKCons[K], FunctionKCons[K]]


  type Any2K[A] = AnyK[Any][A]
  type Scal2K[F[_]] = ScalK[Any, F]
  type Function2K[F[_], G[_]] = FunctionK[Any, F, G]

  type Kleisli[K <: AnyKind, ->[A <: K, B <: K], F[A <: K] <: K, A <: K, B <: K] = A -> F[B]

  type Presheaf[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], F[A <: K]] =
    ScalContravariant[K, Ob, ->, F]

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
