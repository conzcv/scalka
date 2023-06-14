package scalka.kernel

final case class Morphism[
  Kind <: AnyKind,
  Ob[A <: Kind],
  Arr[A <: Kind, B <: Kind],
  A <: Kind, B <: Kind
](domain: Ob[A], arrow: Arr[A, B], codomain: Ob[B])

object Morphism {
  def fromArrow[K <: AnyKind, Ob[A <: K], Arr[A <: K, B <: K], A <: K, B <: K](arr: Arr[A, B])(using Ob[A], Ob[B]): Morphism[K, Ob, Arr, A, B] =
    Morphism(summon, arr, summon)
}

extension [K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], A <: K, B <: K](mor: Morphism[K, Ob, ->, A, B]) {
  def >>[C <: K](f: Morphism[K, Ob, ->, B, C])(using C: Cat[K, Ob, ->]): Morphism[K, Ob, ->, A, C] = C.compose(f, mor)
}