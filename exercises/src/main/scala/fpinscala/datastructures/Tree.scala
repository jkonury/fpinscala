package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]) : Int =
    t match {
      case Branch(left, right) => size(left) + size(right)
      case Leaf(_) => 1

    }

  def maximum(t: Tree[Int]) : Int =
    t match {
      case Branch(left, right) => maximum(left) max maximum(right)
      case Leaf(x: Int) => x
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Branch(left, right) => size(left) max size(right)
      case Leaf(_) => 1
    }


}