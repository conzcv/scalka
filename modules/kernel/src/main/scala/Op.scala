package scalka.kernel

final case class Op[->[_, _], A, B](opposite: B -> A)
