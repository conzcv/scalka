package scalka.instances.category

import scalka.kernel.types._
import scalka.kernel.{Category, FunctionK}

trait ScalKInstances {
  given [K <: AnyKind]: CatK[K] =
    new Category[AnyK[K], ScalKCons[K], FunctionKCons[K]] {
      def id[F[A <: K]: ScalKCons[K]]: FunctionK[K, F, F] =
        FunctionK.id[K, F]

      def compose[
        F[A <: K]: ScalKCons[K],
        G[A <: K]: ScalKCons[K],
        H[A <: K]: ScalKCons[K]
      ](f: FunctionK[K, G, H], g: FunctionK[K, F, G]): FunctionK[K, F, H] =
        g andThen f
  }
}
