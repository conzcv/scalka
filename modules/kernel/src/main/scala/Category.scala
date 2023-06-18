package scalka.kernel

import scalka.instances.category.CategoryInstances

trait Category[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]] {
  def compose[A <: K: Ob, B <: K: Ob, C <: K: Ob](f: B -> C, g: A -> B): A -> C 
  def id[A <: K: Ob]: A -> A
}

object Category extends CategoryInstances {
  def apply[K <: AnyKind, Ob[A <: K], ->[A <: K, B <: K]](using C: Category[K, Ob, ->]): Category[K, Ob, ->] = C
}