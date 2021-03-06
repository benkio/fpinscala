package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1 : Int, a2 : Int) : Int = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1 : Int, a2 : Int) : Int = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1 : Boolean, a2 : Boolean) : Boolean = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1 : Boolean, a2 : Boolean) : Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1 : Option[A], a2 : Option[A]) : Option[A] = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1 : A => A, f2 : A => A) : A => A = (a : A) => f2(f1(a))
    val zero = (a : A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x : A, y : A) : A = m.op(y, x)
    val zero = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = new Monoid[String] {
    def op(s1 : String, s2 : String) : String = List(s, s1, s2).foldLeft(zero)((x, y) => x.trim + " " + y.trim).trim
    val zero : String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) m.zero
    else if (as.length == 1) f(as(0))
      else {
      val (p1, p2) = as.splitAt((as.length/2))
      m.op(foldMapV(p1, m)(f), foldMapV(p2, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val intOrderedMonoid : Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      val zero = None
      def op(p : Option[(Int, Int,Boolean)], n : Option[(Int, Int, Boolean)]) : Option[(Int, Int, Boolean)] = (p, n) match {
        case (Some((x1, y1, z1)), Some((x2, y2, z2))) => {
          if (x1 <= x2 && y1 <= x2 && z1 && z2) // Ordered
            Some((x1, y2, true))
          else
            Some((x1, y2, false))
        }
        case (None, x) => x
        case (x, None) => x
      }
    }

    foldMapV(ints, intOrderedMonoid)(x => Some((x, x, true))) match {
      case None => true
      case Some((_, _, b : Boolean)) => b
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero : WC = Stub("")
    def op(m1 : WC, m2 : WC): WC = (m1, m2) match {
      case (Stub(s1), Stub(s2))                     => Stub(s1 + s2)
      case (Stub(s1), Part(ls, c, rs))              => Part((s1 + ls), c, rs)
      case (Part(ls, c, rs), Stub(s1))              => Part(ls, c, (rs + s1))
      case (Part(ls1, c1, rs1), Part(ls2, c2, rs2)) => Part(ls1, (c1 + c2 + (if ((rs1 + ls2).isEmpty) 0 else 1)), rs2)
    }
  }

  def count(s: String): Int = {
    def toMonoid(s : String) : WC = {
      val sSplitted = s.split(" ")
      if (sSplitted.size > 0) Part(sSplitted.head, sSplitted.size - 2, sSplitted.last)
      else wcMonoid.zero
    }

    def toInt(w : WC) : Int = w match {
      case Stub(s) => if (s.isEmpty) 0 else 1
      case Part(_, c, _) => c
    }

    s.grouped(5)
      .map(toMonoid)
      .sliding(2, 2)
      .map(l => (l.head, l.last))
      .map { case (m1, m2) => toInt(wcMonoid.op(m1, m2)) }
      .fold(0)(_ + _)
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      val zero : (A, B) = (A.zero, B.zero)
      def op(a: (A, B), b: (A, B)) : (A, B)  =
        (A.op(a._1, b._1), B.op(a._2, b._2))
    }

  /* Different implementations and  */
  def coproductMonoid[A, B](A : Monoid[A],
                            B : Monoid[B]) : Monoid[Either[A, B]] =
    new Monoid[Either[A, B]] {
      val zero = ???
      def op(a : Either[A, B], b : Either[A, B]) : Either[A, B] =
        ???
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      val zero = A => B.zero
      def op(a : A => B, b : A => B) : A => B =
        x => B.op(a(x), b(x))
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      val zero : Map[K, V] = Map[K, V]()
      def op(m1 : Map[K, V], m2 : Map[K, V]) : Map[K, V] =
        m1.map {
          case (k, v) => (k, V.op(v, m2.get(k).getOrElse(V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)((a : A) => (b : B) => f(b, a))(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(b,f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)((a : A) => m.op(m.zero, a))(m)

  def toList[A](as: F[A]): List[A] =
    foldMap(as)(a => List(a))(listMonoid)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).foldRight(mb.zero)(mb.op)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).foldLeft(mb.zero)(mb.op)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).foldLeft(mb.zero)(mb.op)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(v) => f(z,v)
    case Branch(r,l) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(v) => f(v, z)
    case Branch(r,l) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(x) => f(x)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(x) => f(z, x)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(x) => f(x, z)
  }
}
