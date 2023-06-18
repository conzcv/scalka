package scalka.instances.category

import scalka.kernel.types._
import scalka.kernel.{Category, Monad, Op}
import scalka.syntax.category.* 

trait CategoryInstances extends ScalInstances {
  given [Ob[_], ->[_, _]](using C: Category[Ob, ->]): Category[Ob, [A, B] =>> Op[->, A, B]] =
    new Category[Ob, [A, B] =>> Op[->, A, B]] {
      def compose[A: Ob, B: Ob, C: Ob](f: Op[->, B, C], g: Op[->, A, B]): Op[->, A, C] =
        Op(f.opposite >>> g.opposite)
      def id[A: Ob]: Op[->, A, A] = Op(C.id[A])
    }

  given [Ob[_], ->[_, _], F[_]](using C: Category[Ob, ->], M: Monad[Ob, ->, F]): KleisliCat[Ob, ->, F] =
    new KleisliCat[Ob, ->, F] {
      def compose[A: Ob, B: Ob, C: Ob](f: Kleisli[->, F, B, C],  g: Kleisli[->, F, A, B]): Kleisli[->, F, A, C] =
        given fb: Ob[F[B]] = M.apply[B]
        given fc: Ob[F[C]] = M.apply[C]
        C.compose(M.bind(f), g)

      def id[A: Ob]: Kleisli[->, F, A, A] =
        M.pure.relation[A]
    }
}

