package scalka.kernel

import scalka.instances.category.CategoryInstances

trait Category[Ob[_], ->[_, _]] {
  def compose[A: Ob, B: Ob, C: Ob](f: B -> C, g: A -> B): A -> C 
  def id[A: Ob]: A -> A
}

object Category extends CategoryInstances {
  def apply[Ob[_], ->[_, _]](using C: Category[Ob, ->]): Category[Ob, ->] = C
}