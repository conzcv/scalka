package scalka.kernel

trait Contravariant[
  S <: AnyKind, SOb[A <: S], ->[A <: S, B  <: S],
  D <: AnyKind, DOb[A <: D], ~>[A <: D, B <: D],
  F[A <: S] <: D
] extends Functor[S, SOb, [A <: S, B <: S] =>> Op[S, ->, A, B], D, DOb, ~>, F] {
  def contamap[A <: S: SOb, B <: S: SOb](f: A -> B): F[B] ~> F[A] = fmap(Op(f))
}
