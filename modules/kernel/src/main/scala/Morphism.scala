package scalka.kernel

final case class Morphism[
  K <: AnyKind,
  Ob[A <: K],
  Rel[A <: K, B <: K],
  A <: K, B <: K
](domain: Ob[A], arrow: Rel[A, B], codomain: Ob[B]) { self =>
  def >>[C <: K](f: Morphism[K, Ob, Rel, B, C])(using C: Cat[K, Ob, Rel]): Morphism[K, Ob, Rel, A, C] = C.compose(f, self)
}

object Morphism {
  def fromArrow[K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], A <: K, B <: K](arr: Rel[A, B])(using Ob[A], Ob[B]): Morphism[K, Ob, Rel, A, B] =
    Morphism(summon, arr, summon)
}