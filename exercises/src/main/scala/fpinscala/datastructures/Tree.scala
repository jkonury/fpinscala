package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }


  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(2))
    val t2 = Branch(Leaf(1), Leaf(2))

    println(size(t))
    println(maximum(t))
    println(depth(t2))

  }
}