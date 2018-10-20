package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    // Int.MinVlaue has no positive equivalent, so +1
    case (r, gen) if r < 0 => (-1*r + 1, gen)
    case a => a
  }

  def double(rng: RNG): (Double, RNG) = {
    // n.b., similar tuple unpacking as python
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  def doubleMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, rr) = double(rng)
    ((i, d), rr)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((i, d), rng) => ((d, i), rng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // A simple recursive solution (from solutions, didn't think of this)
  def intsSimpleRec(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  def ints(count: Int)(rng: RNG): (List[Int],RNG) = {
    @annotation.tailrec
    def loop(c: Int)(acc: (List[Int], RNG)): (List[Int], RNG) = acc._2.nextInt match {
      case (i, rr) if c > 0 =>  loop(c - 1)((i :: acc._1, rr))
      case _ => acc
    }

    loop(count)((List(), rng))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    // map(ra)(a => map(rb)(b => f(a, b))) doesn't work bc state change to RNG
    // that gets passed in doesn't propagate to map(rb)
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def map2FlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int,Double)] = both(int, doubleMap)

  def randDoubleInt: Rand[(Double,Int)] = both(doubleMap, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    // note use of `unit` for the `z` argument in answerkey solution
    fs.foldRight((rz: RNG) => (List[A](),rz))(
      // in hindsight, this looks almost identical to the definition of `map2`!
      (a, b) => r => {
        val (aa, r2) = a(r)
        val (bb, r3) = b(r2)
        (aa :: bb, r3)
      }
    )

  // https://github.com/fpinscala/fpinscala/blob/master/answerkey/state/07.answer.scala
  //
  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  def sequenceAnswerKey[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  // reimplement `ints` using `sequence`
  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (fi, fr) = f(rng)
      g(fi)(fr)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb map (b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    }
  )

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((a, b) => a.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
