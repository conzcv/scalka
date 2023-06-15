package scalka.kernel

import scalka.syntax.functionK.functionK

trait FlatMap[
  K <: AnyKind,
  Ob[A <: K],
  Arr[A <: K, B <: K],
  F[A <: K] <: K
] extends Endofunctor[K, Ob, Arr, F] {
  type Flatten[A <: K] = From[F o F, A]
  
  given category: Cat[K, Ob, Arr]

  val flatten: Transform[F o F, F] =
    val function = functionK[K, Ob, Flatten](ob => flatMap(ob)(fmap(ob.id[Arr])))
    Nat(function)

  final def flatMap[A <: K, B <: K](ob: Ob[B])(f: A -> F[B]): F[A] -> F[B] =
    fmap(f) >> flatten(ob)
}