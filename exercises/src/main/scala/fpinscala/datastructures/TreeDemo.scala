package fpinscala.datastructures

import fpinscala.datastructures.Tree._

object TreeDemo extends App {

  println(size(Leaf("a")))
  println(size(Branch(Leaf("a"), Leaf("b"))))
  println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))))


  println(maximum(Leaf(1)))
  println(maximum(Branch(Leaf(1), Leaf(2))))
  println(maximum(Branch(Leaf(3), Leaf(2))))

  println(maximum(Branch(Branch(Leaf(10), Leaf(1)), Branch(Leaf(5), Leaf(7)))))
  println(maximum(Branch(Branch(Leaf(6), Leaf(7)), Branch(Leaf(3), Branch(Leaf(1), Leaf(2))))))

}
