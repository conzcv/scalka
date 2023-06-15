package scalka.kernel

final case class Morphism[
  K <: AnyKind,
  Ob[A <: K],
  Rel[A <: K, B <: K],
  A <: K, B <: K
](domain: Ob[A], arrow: Rel[A, B], codomain: Ob[B]) { self =>
  def >>[C <: K](f: Morphism[K, Ob, Rel, B, C])(using C: Category[K, Ob, Rel]): Morphism[K, Ob, Rel, A, C] = C.compose(f, self)
}

object Morphism {
  def fromRelation[K <: AnyKind, Ob[A <: K], Rel[A <: K, B <: K], A <: K: Ob, B <: K: Ob](arr: Rel[A, B]): Morphism[K, Ob, Rel, A, B] =
    Morphism(summon, arr, summon)
}