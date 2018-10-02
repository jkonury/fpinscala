package fpinscala

object Demo extends App {
  val l = List(1, 2, 3)
  val l1 = List()
  val l2 = List(1)

  println(l.tail)
//  println(l1.tail)
  println(l2.tail)

  println(l.dropWhile(_ == 2))

  println(l(1))
}
