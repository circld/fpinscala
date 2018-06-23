package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{None => _, Option => _, Some => _, Either => _, _}

// illustrative example demonstrating diff b/w map + flatmap (in Option context)
// http://seanparsons.github.io/scalawat/Using+flatMap+With+Option.html
//
//case class Player(name: String)
// def lookupPlayer(id: Int): Option[Player] = {
//   if (id == 1) Some(new Player("Sean"))
//   else if(id == 2) Some(new Player("Greg"))
//   else None
// }
// def lookupScore(player: Player): Option[Int] = {
//   if (player.name == "Sean") Some(1000000) else None
// }
//
// println(lookupPlayer(1).map(lookupScore))  // Some(Some(1000000))
// println(lookupPlayer(2).map(lookupScore))  // Some(None)
// println(lookupPlayer(3).map(lookupScore))  // None
//
// println(lookupPlayer(1).flatMap(lookupScore))  // Some(1000000)
// println(lookupPlayer(2).flatMap(lookupScore))  // None
// println(lookupPlayer(3).flatMap(lookupScore))  // None

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    // since flatMap applies map, function only applied when Some...
    flatMap(a => if(f(a)) Some(a) else None)
    // original solution
    // if(map(f) getOrElse false) this else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
