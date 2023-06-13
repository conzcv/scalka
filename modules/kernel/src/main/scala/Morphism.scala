package scalka.kernel

final case class Morphism[
  Kind <: AnyKind,
  Ob[A <: Kind],
  Arr[A <: Kind, B <: Kind],
  A <: Kind, B <: Kind
](domain: Ob[A], arrow: Arr[A, B], codomain: Ob[B])

extension [K <: AnyKind, ->[A <: K, B <: K], A <: K, B <: K](arr: A -> B) {
  def toMorphism[Ob[A <: K]](domain: Ob[A], codomain: Ob[B]): Morphism[K, Ob, ->, A, B] =
    Morphism[K, Ob, ->, A, B](domain, arr, codomain)
}

extension [K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K], A <: K, B <: K](mor: Morphism[K, Ob, ->, A, B]) {
  def >>[C <: K](f: Morphism[K, Ob, ->, B, C])(using C: Cat[K, Ob, ->]): Morphism[K, Ob, ->, A, C] = C.compose(f, mor)
}