package scalka.kernel

trait Endofunctor[
  Kind <: AnyKind,
  Ob[A <: Kind],
  Arr[A <: Kind, B <: Kind],
  F[A <: Kind] <: Kind
] extends Fun[Kind, Ob, Arr, Kind, Ob, Arr, F] {
  final type ~>[A <: Kind, B <: Kind] = ->[A, B]
}

