package scalka.kernel

import scalka.kernel.Morphism
import scalka.kernel.types.IdK
import scalka.syntax.functionK.functionK

trait Adjunction[
  S <: AnyKind, SOb[A <: S], SArr[A <: S, B <: S],
  D <: AnyKind, DOb[A <: D], DArr[A <: D, B <: D],
  R[A <: S] <: D, L[A <: D] <: S
] {
  type ->>[A <: S, B <: S] = Morphism[S, SOb, SArr, A, B]
  type ~>>[A <: D, B <: D] = Morphism[D, DOb, DArr, A, B]
  type Unit[A <: D] = A ~>> R[L[A]]
  type Counit[A <: S] = L[R[A]] ->> A
  type RL[A <: D] = R[L[A]]
  type LR[A <: S] = L[R[A]]

  given S: Cat[S, SOb, SArr]
  given D: Cat[D, DOb, DArr]
   
  val R: Fun[S, SOb, SArr, D, DOb, DArr, R]
  val L: Fun[D, DOb, DArr, S, SOb, SArr, L]

  def right[A <: D, B <: S](ob: DOb[A])(f: L[A] ->> B): A ~>> R[B]
  def left[A <: D, B <: S](ob: SOb[B])(f: A ~>> R[B]): L[A] ->> B
  

  def unit: Nat[D, DOb, D, DOb, DArr, IdK[D], RL] =
    val funk = functionK[D, DOb, Unit](ob => right(ob)(L.fmap(ob.id[DArr])))
    Nat[D, DOb, D, DOb, DArr, IdK[D], RL](funk)

  def counit: Nat[S, SOb, S, SOb, SArr, LR, IdK[S]] =
    val funk = functionK[S, SOb, Counit](ob => left(ob)(R.fmap(ob.id[SArr])))
    Nat[S, SOb, S, SOb, SArr, LR, IdK[S]](funk)

  def monad: Monad[D, DOb, DArr, RL] =
    new Monad[D, DOb, DArr, RL]  {
      val category = D
      val pure: Transform[IdK[D], RL] = unit

      val flatten: Transform[[A <: D] =>> RL[RL[A]], RL] =
        new Transform[[A <: D] =>> RL[RL[A]], RL] {
          def apply[A <: D](ob: DOb[A]): RL[RL[A]] ~> RL[A] =
            val id = ob.id[DArr]
            R.fmap(left(L.fmap(id).domain)(fmap(id)))
        }

      def fmap[A <: D, B <: D](f: A ~>> B): RL[A] ~>> RL[B] =
        R.fmap(L.fmap(f))
    }

  def comonad: Comonad[S, SOb, SArr, LR] =
    new Comonad[S, SOb, SArr, LR] {
      val category = S
      val extract: Transform[LR, IdK[S]] = counit

      val coflatten: Transform[LR, [A <: S] =>> LR[LR[A]]] =
        new Transform[LR, [A <: S] =>> LR[LR[A]]]  {
          def apply[A <: S](ob: SOb[A]): LR[A] ~> LR[LR[A]] =
            val id = ob.id[SArr]
            L.fmap(right(R.fmap(id).codomain)(fmap(id)))
        }

      def fmap[A <: S, B <: S](f: A ->> B): L[R[A]] ->> L[R[B]] =
        L.fmap(R.fmap(f))
    }
}
