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

  def productLF(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((a, b) => a*b)

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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendLF[A](a1: List[A], a2: List[A]): List[A] =
    List.foldLeft(a1, (z: List[A]) => z)((g, a) => (b) => g(Cons(a, b)))(a2)

  // List.foldRight(testList, List[Int]())(Cons(_, _))
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def sumLF(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Nil has no tail.")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => if (n == 0) l else drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Nil has no tail.")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => 1 + length(xs)
  }

  // List.foldLeft(testList, List[Int]())((a, b) => Cons(b, a))
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // List.foldRight(testList, List[Int]())(Cons(_, _))
  // idea: correct reversing of order by composing function applications
  //       accumulator becomes identity func (accumulation now in composed func)
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (z: B) => z)((g, a) => (b: B) => g(f(a, b)))(z)

  // tail recursive variant
  // List.foldRight2(testList, List[Int]())(Cons(_, _))
  def mapTRO[A,B](l: List[A])(f: A => B): List[B] =
    // foldRight2(l, List[B]())((h: A, t: List[B]) => Cons(f(h), t))
    foldLeft(l, (z: List[B]) => z)(
      (g, h) => (t: List[B]) => g(Cons(f(h), t)))(Nil)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((a, b) => Cons(b, a))

  // List.filter(testList)(_ % 2 == 0)
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    // foldLeft(as, (z: List[A]) => z)((g, a) => if (f(a)) (b) => g(Cons(a, b)) else g)(List[A]())
    foldRight2(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def filterStackUnsafe[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  // w/linear runtime in total length of all lists
  // n.b., (appendLF) == (_ appendLF _) == ((a, b) => append(a, b))
  // in this case; last may be needed to help type inference
  def concat[A](l: List[List[A]]): List[A] =
    foldRight2(l, List[A]())(appendLF)

  // can use foldLeft bc function is commutative
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    // foldLeft(map(as)(f), List[B]())(append)
    concat(map(as)(f))

  // implement filter using flatMap
  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if (f(a)) List(a) else Nil)

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // idea: zipWith variant returning function doing the zipping
  def zipWithTRO[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    def loop[A](xs: List[A], ys: List[A])(f1: (A, A) => A)
      (g: List[A] => List[A]): List[A] => List[A] = {
      (xs, ys) match {
        case (Nil, _) => g
        case (_, Nil) => g
        case (Cons(c, cs), Cons(d, ds)) => loop(cs, ds)(f1)((h: List[A]) => g(Cons(f1(c, d), h)))
      }
    }

    loop(as, bs)(f)((a) => a)(Nil)
  }

}
