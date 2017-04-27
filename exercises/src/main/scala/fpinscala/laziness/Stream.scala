package fpinscala.laziness

import Stream._
trait Stream[+A] {

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


  def toList : List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def toListTailRec : List[A] = {
    @annotation.tailrec
    def go[A](s : Stream[A], acc : List[A]) : List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, Nil).reverse
  }

  def take(n: Int) : Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n-1))
    case _ => Empty
//    case _ if (n == 0) => Empty
//    case _ =>  throw new Exception("stream take: n negative or higher than stream size")
  }

  def takeViaUnfold(n : Int) : Stream[A] =
    unfold((n, this)) {
      case (x, Cons(h, t)) if (x > 0) => Some(h(), (x - 1, t()))
      case _                          => None
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a, b) else empty )

  def takeWhileViaUnfold(p : A => Boolean) : Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h1, t1)) => Some (f(h(), h1()), (t(), t1()))
      case _ => None
    }

  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h1, t1)) => Some (f(Some(h()), Some(h1())), (t(), t1()))
      case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (empty, t()))
      case _ => None
    }

  def zip[B](s : Stream[B]) : Stream[(A,B)] =
    zipWith(s)((x, y) => (x, y))

  def zipAll[B](s : Stream[B]) : Stream[(Option[A],Option[B])] =
    zipWithAll(s)((x, y) => (x, y))

  def forAll(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def go(s : Stream[A], p: A => Boolean) : Boolean = s match {
      case Cons(h, t) if (p(h())) => go(t(), p)
      case Empty => true
      case _     => false
    }

    go(this, p)
  }

  def mapViaUnfold[B](f : A => B) : Stream[B] =
    Stream.unfold(this)(s => s match {
                          case Cons(h, t) => Some(f(h()), t())
                          case Empty      => None
                        })

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def filter(f : A => Boolean) : Stream[A] =
    foldRight[Stream[A]](empty[A])((a, b) => if (f(a)) cons(a , b) else b)

  def append[B>:A](s : => Stream[B]) : Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B>:A](f : A => Stream[B]) : Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => f(a) append b)

  def map[B](f : A => B) : Stream[B] =
    foldRight[Stream[B]](empty[B])((a, b) => cons(f(a), b))

  // Not elegant but works
  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h, t), Cons(h1, t1)) if (h() == h1())    => t1() match {
      case Empty => true
      case _ => t().startsWith(t1())
    }
    case (Empty, Empty)                                 => true
    case _                                              => false
  }

  def tails : Stream[Stream[A]] =
    unfold((this, true))(s => s match {
                           case (Cons(h, t), b)  => Some((Cons(h, t), (t(), b)))
                           case (Empty, true)    => Some((Empty, (Empty, false)))
                           case _                => None
                         } )

  def hasSubsequence[A](s : Stream[A]) : Boolean =
    this.tails exists (_.startsWith(s))


  def scanRight[B](z : B)(f : (A, => B) => B) : Stream[B] =
    this.tails map (s => s match {
                      case Empty => z
                      case _ => s.foldRight(z)(f)
                    })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones                   : Stream[Int] = Stream.cons(1, ones)
  def constant[A](value : A) : Stream[A]   = Stream.cons(value, constant(value))
  def from(n : Int)          : Stream[Int] = Stream.cons(n, from(n+1))

  val fibs : Stream[Int] = {
    def go(x : Int, y : Int) : Stream[Int] = Stream.cons((x), go(y, (x+y)))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(t)    => Stream.cons(t._1, unfold(t._2)(f))
    case None       => Empty
  }

  val ones2                   : Stream[Int] = unfold(1)    (x => Some(x, x))
  def constant2[A](value : A) : Stream[A]   = unfold(value)(x => Some(x, x))
  def from2(n : Int)          : Stream[Int] = unfold(n)    (x => Some(x, x+1))
  val fibs2                   : Stream[Int] = unfold((0, 1)){ case (x, y) => Some((x), (y, (x + y)))}
}
