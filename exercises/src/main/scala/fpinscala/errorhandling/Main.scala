package fpinscala.errorhandling

case class Person1(name: String, age: Int)

object Main extends App {

  val p = Person1(name = "Hong", age = 20)

  def lookup(name: String): Option[Person1] =
    if (p.name == name)
      Some(p)
    else
      None

  println(lookup("hong").map(_.age))
  println(lookup("Hong").map(_.age))
  println(lookup("hong").flatMap(Some(_)))
  println(lookup("Hong").flatMap(Some(_)))
  println(lookup("hong").map(_.age).getOrElse(Person1("Person1", 10)))

  val list: List[Option[Int]] = List(Some(10), Some(30), Some(20))
  println(list)
  println(Option.sequence(list))
}