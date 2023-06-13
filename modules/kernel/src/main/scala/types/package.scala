package scalka.kernel

package object types {
  type Id[+A] = A
  type IdK[K <: AnyKind] = [A <: K] =>> A
  type AnyK = [A] =>> Any

  sealed trait Scal[A]
  object Scal:
    private val impl = new Scal[Any] {}
    given [A]: Scal[A] = impl.asInstanceOf[Scal[A]]

  sealed trait ScalK[F[_]]
  object ScalK:
    private val impl = new ScalK[AnyK] {}
    given [F[_]]: ScalK[F] = impl.asInstanceOf[ScalK[F]]

  type FunctorCat[
    SKind <: AnyKind, SOb[A <: SKind], SArr[A <: SKind, B <: SKind],
    DKind <: AnyKind, DOb[A <: DKind], DArr[A <: DKind, B <: DKind]
  ] = Cat[
    [A <: SKind] =>> DKind,
    [F[A <: SKind] <: DKind] =>> Fun[SKind, SOb, SArr, DKind, DOb, DArr, F],
    [F[A <: SKind] <: DKind, G[A <: SKind] <: DKind] =>> Nat[SKind, SOb, DKind, DOb, DArr, F, G]
  ]

  type EndoNat[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K], F[A <: K] <: K, G[A <: K] <: K] =
    Nat[K, Ob, K, Ob, Arr, F, G]
}
