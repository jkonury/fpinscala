package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  val list: List[Int] = List(1, 2, 3, 4, 5)

  val x1: List[Int] = list match {
    case Cons(h, t) if h > 2 => t
    case Cons(h, t) => t
    case _ => List(1)
  }

  def main(args: Array[String]): Unit = {
    println(x)
    println(tail(List()))
    println(tail(List(1, 2)))
    println(setHead(List(), 1))
    println(setHead(List(1, 2), 3))

    println(x1)
    println(init(list))

    val ex1 = dropWhile(list, (x: Int) => x < 4)
//    val ex2 = dropWhile(list)(x => x < 4)

    println(length(list))

    // foldRight(ns, 0)((x,y) => x + y)
    println(foldLeft(list, 0)((x, y) => {
      println(s"$x $y")
      x + y
    } ))
    println(foldLeft(list, 1)(_ * _))

    val a = foldLeft(list, Nil: List[Int]) ((x, y) => {
      println(s"$x $y")
      Cons(y, x)
    })

//    foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
    println(a)
    println(foldRight(list, Nil:List[Int])((x, y) => Cons(x, y)))

    val list1 = List(1, 2)
    val list2 = List(3, 4)

    println("\n\n\n")
    println(appendFoldLeft(list1, list2))
    println(appendFoldRight(list1, list2))
    println(append(list1, list2))



    println("\nadd1\n")

//    def add1(xs: List[Int]): List[Int] = foldLeft(xs, Nil: List[Int])((x, y) => {
//      Cons(y + 1, x)
//    })

    def add1(l: List[Int]): List[Int] =
      foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

    def add2(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((x, y) => {
      Cons(x + 1, y)
    })

    println(list)
    println(add1(list))
    println(add2(list))
    //    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

//    def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
//    def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  }


  def appendFoldLeft[A](left: List[A], right: List[A]): List[A] =
    foldLeft(left, right)((x, y) => {
      println(s"$x $y")
      Cons(y, x)
    })

  def appendFoldRight[A](left: List[A], right: List[A]): List[A] =
    foldRight(left, right)(Cons(_, _))
//    foldRight(left, right)((x, y) => Cons(x, y))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

//  @scala.annotation.tailrec
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//      case Cons(x, xs) => foldRight(xs, f(x, z))(f)
    }

  @scala.annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n -1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }


//  @scala.annotation.tailrec
//  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
//    case Cons(h, t) if f(h) => dropWhile(t)(f)
//    case _ => xs
//  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => {
      println(s"$x $y")
      y + 1
    })



  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
