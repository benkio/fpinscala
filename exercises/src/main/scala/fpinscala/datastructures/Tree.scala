package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree : Tree[A]) : Int = tree match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
  }

}
