package scalka.kernel

final case class Morphism[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  A <: K, B <: K
](domain: Ob[A], arrow: Arr[A, B], codomain: Ob[B]) { self =>
  def >>[C <: K](f: Morphism[K, Ob, Arr, B, C])(using C: Cat[K, Ob, Arr]): Morphism[K, Ob, Arr, A, C] = C.compose(f, self)
}

object Morphism {
  def fromArrow[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K], A <: K, B <: K](arr: Arr[A, B])(using Ob[A], Ob[B]): Morphism[K, Ob, Arr, A, B] =
    Morphism(summon, arr, summon)
}