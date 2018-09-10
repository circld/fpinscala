package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // rercursive
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // tail call variant
  // n.b., inspired by TRO foldRight implementation from datastructures
  def toListTailRec: List[A] = {
    @annotation.tailrec
    def loop[B](as: Stream[B], g: List[B] => List[B]): List[B] => List[B] =
      as match {
        case Empty => g
        case Cons(h, t) => loop(t(), (b) => g(h() :: b))
      }

    loop(this, (a: List[A]) => a)(List[A]())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // just because this is lazy, not tail recursive so can still stack overflow?
  // it returns a _stream_, so "yields" only an element at a time; only time to
  // worry about SO is when everything must be evaluated (e.g., find, toList)
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>  cons(h(), t() takeWhile p)
    case _ => empty
  }


  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }


  // if stream is empty, function will not be called and z (None) gets returned
  def headOptionFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// how do thing defined on the trait v on the companion object work?
// are they additive (is there no distinction from the user's perspective?)
object Stream {
  // "smart" constructor w/caching
  // n.b., declares Stream[A] type on tail to avoid default of subtypes (e.g.,
  // Cons)
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
