package scalka.kernel

final case class Morphism[
  K <: AnyKind,
  Ob[A <: K],
  Rel[A <: K, B <: K],
  A <: K, B <: K
](domain: Ob[A], relation: Rel[A, B], codomain: Ob[B])

final private[kernel] class FromRelationPartiallyApplied[K <: AnyKind, Ob[A <: K]] (private val __ : Boolean) extends AnyVal {
  def apply[Rel[A <: K, B <: K], A <: K: Ob, B <: K: Ob](arr: Rel[A, B]): Morphism[K, Ob, Rel, A, B] =
    Morphism(summon, arr, summon)
}

object Morphism {
  def fromRelation[K <: AnyKind, Ob[A <: K]]: FromRelationPartiallyApplied[K, Ob] =
    new FromRelationPartiallyApplied(true)
}