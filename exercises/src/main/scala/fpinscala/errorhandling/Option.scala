package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  // : => call by name
  // B >: A   B가 A의 상위 클래스
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def orElse1[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => ob
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def filter(f: A => Boolean): Option[A] = this match  {
    case Some(a) if(f(a)) => this
    case None => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => (b map (y => f(x, y))))

  def map2For[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)


  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
    // def map[B](f: A => B): Option[B] = this match {
    // def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
//    a map(x => x flatMap(y => )

//    a flatMap(x => x map (y => ))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def insuranceRateQuote(optAge: Int, optTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTicket: String): Option[Double] = {
    val optAge: Option[Int] = Try {age.toInt}
    val optTickets: Option[Int]= Try {numberOfSpeedingTicket.toInt}
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
}