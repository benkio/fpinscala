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

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      case (x ,r) if (x >= 0) => (x, r)
      case (Int.MinValue, r)  => (Int.MaxValue - 1, r)
      case (x, r)             => (x.abs, r)
  }


  def double(rng: RNG): (Double, RNG) = {
    val (x, r) = RNG.nonNegativeInt(rng)
    ((x / Int.MaxValue), r)
  }

  val double : Rand[Double] =
    map(int)(_.toDouble)

  // Did not reuse double becouse of the nonNegativeInt.
  // It wasn't specified in the book to filter out the negative numbers.

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (x, r)  = rng.nextInt
    val (y, r1) = r.nextInt
    ((x, y.toDouble), r1)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((x, y), r) = intDouble(rng)
    ((y, x), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (x, r)  = rng.nextInt
    val (y, r1) = rng.nextInt
    val (z, r2) = rng.nextInt
    ((x.toDouble, y.toDouble, z.toDouble), r2)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, acc : List[Int])(rng : RNG) : (List[Int], RNG) =
      if (count == 0) (acc, rng)
      else {
        val (x, r) = rng.nextInt
        go(count-1, x :: acc)(r)
      }

    go(count, List[Int]())(rng)
  }

  //  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  def positiveMax(n : Int) : Rand[Int] =
    map(r => {
          val (x, r1) = r.nextInt
          if (x < n) (x, r1)
          else positiveMax(n)(r1)
        })(x => x)

  def positiveMax2(n : Int) : Rand[Int] =
    map(nonNegativeInt)(_ % n)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
