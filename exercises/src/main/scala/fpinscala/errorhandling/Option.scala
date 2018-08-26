package fpinscala.errorhandling
import scala.annotation.tailrec

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{None => _, Option => _, Some => _, Either => _, _}

// map vs flatMap
// map
// - returns Some if input is present, None otherwise
// - if function returns Option, then returns Some(Some) or Some(None)
// flatMap (map + flatten, work over nesting)
// - if function returns Option, does the right thing (Some or None)

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

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map ((x) => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (f(aa, _: B)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def loop[A](b: List[Option[A]])(c: List[A]): List[A] = b match {
      case Nil => c
      case None :: _ => Nil
      case Some(x) :: tail => loop(tail)(x :: c)
    }

    loop(a)(List[A]()) match {
      case Nil => None
      case x => Some(x)
    }
  }

  // this is pretty mind-melting; foldRight + map2 + options
  // n.b., the List[Option[B]] -> Option[List[B]] is happening from `map2`
  // "lifting" the list concat operator `::` from:
  // A :: List[A] => List[A]
  // to Option[A] :: Option[List[A]]) => Option[List[A]]
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequence_trav[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}
