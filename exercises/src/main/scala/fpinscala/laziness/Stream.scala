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

  def takeUnfold(n: Int): Stream[A] =
    unfold((n, this))(
      (s: (Int, Stream[A])) => s match {
        case (a, Cons(h, t)) if a > 0 => Some(h(), (a - 1, t()))
        case _ => None
      }
    )

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

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this)({case Cons(h, t) => if (p(h())) Some((h(), t())) else None})

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
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) cons(a, b) else b)

  // the resulting type has to be a supertype
  def append[B>:A](as: Stream[B]): Stream[B] = foldRight(as)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((a, b) => f(a) append b)

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(h, t), Cons(bh, bt)) => Some((f(h(), bh()), (t(), bt())))
      case _ => None
    }

  // continues traversal as long as one stream is productive
  // n.b., could have had f be (A, B) => C; this limits flexibilty for `f` to
  // handle cases when A or B may be empty and a value should still be returned
  // for the other argument
  def zipWithAll[B,C](bs: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(h, t), Cons(bh, bt)) => Some((f(Some(h()), Some(bh())), (t(), bt())))
      case (Cons(h, t), empty) => Some((f(Some(h()), None), (t(), empty)))
      case (empty, Cons(h, t)) => Some((f(None, Some(h())), (empty, t())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_, _))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile({case (a, s) => !s.isEmpty}) forAll {case (a, s) => a == s}

  def tails: Stream[Stream[A]] = unfold(this) {
    // why does this break if `empty` is used instead?
    // `empty` is a method, not a case class, so it's treated as a regular name
    case Empty => None
    case s => Some((s, s drop 1))
  // why does simply Stream() not work here?
  // bc `append` adds the contents to the original stream, so you need to append
  // Stream(Stream()) or Stream(empty)
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  // Stream(1,2,3).scanRight(0)(_ + _).toList
  // List[Int] = List(6, 5, 3, 0)
  //
  // implementing `tails` using `scanRight`:
  // testStream.scanRight(Stream[Int]())((a, b) => cons(a, b)).map(_.toList).toList
  // n.b., cannot cache intermediate calculations
  def scanRightUnfold[B](z: B)(f: (A, => B) => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case s => Some((s.foldRight(z)(f), s drop 1))
    } append Stream(z)

  // how does this caching make a difference v unfold implementation
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val bb = b
      val aa = f(a, bb._1)
      (aa, cons(aa, bb._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// how do things defined on the trait v on the companion object work?
// trait -> instance methods
// obj   -> static methods
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

  val onesUnfold: Stream[Int] = unfold(1)(s => Some((1, s)))

  def constant[A](a: A): Stream[A] = {
    // cons(a, constant(a))
    // more efficient implementation (bc does not result in additional object
    // creation?)
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def constantUnfold[A](a: A): Stream[A] = unfold(a)((b: A) => Some((b, b)))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)((b: Int) => Some(b, b + 1))

  def fibs(): Stream[Int] = {
    def loop(n0: Int, n1: Int): Stream[Int] = cons(n0, loop(n1, n0 + n1))
    loop(0, 1)
  }

  def fibUnfold(): Stream[Int] = {
    val f = (s: (Int, Int)) => Some((s._2, (s._2, s._1 + s._2)))
    cons(0, unfold((0, 1))(f))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some( (a, s) ) => cons(a, unfold(s)(f))
    case None => Stream[A]()
  }

}
