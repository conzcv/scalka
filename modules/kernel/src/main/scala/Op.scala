package scalka.kernel

final case class Op[K <: AnyKind, ->[A <: K, B <: K], A <: K, B <: K](opposite: B -> A)
