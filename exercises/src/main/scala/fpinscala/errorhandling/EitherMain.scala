package fpinscala.errorhandling

case class Person(name: Name, age: Age)

sealed class Name(val value: String) {
  override def toString: String = value
}
sealed class Age(val value: Int) {
  override def toString: String = s"$value"
}

object EitherMain {

  def mkName(name: String): Either[String, Name] =
    if (name == null || name == "")
      Left("Name is Empty")
    else
      Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0)
      Left("Age is out of ")
    else
      Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)


  def mkPerson2For(name: String, age: Int): Either[String, Person] =
    mkName(name).map2For(mkAge(age))(Person)

  def main(args: Array[String]): Unit = {
    println(mkPerson("name", 10))
    println(mkPerson2For("name", 10))
  }
}
