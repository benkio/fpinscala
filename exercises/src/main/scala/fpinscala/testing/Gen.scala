package fpinscala.testing

import fpinscala.laziness.{Stream, Cons, Empty}
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import scala.util.Try
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Status
case object Exhausted   extends Status
case object Proven      extends Status
case object Unfalsified extends Status


case class Prop(run : (MaxSize, TestCases, RNG) => Result) {
  def &&(p : Prop) : Prop = Prop{ (m, t, r) =>
    if (run(m, t, r).isLeft) run(m, t, r) else p.run(m, t, r)
  }

  def ||(p : Prop) : Prop = Prop{ (m, t, r) =>
    if (run(m, t, r).isRight) run(m, t, r) else p.run(m, t, r)
  }

  def tag(msg : String) : Prop =
    Prop{ (m, t, r) => run(m, t, r) match {
           case Left(s) => Left(msg + s)
           case x => x
         }
    }
}

object Prop {
  type TestCases    = Int
  type MaxSize      = Int
  type Size         = Int
  type FailedCase   = String
  type SuccessCount = Int
  type Result       = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n,rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd : Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s match {
          case Cons(h,t) => h() match {
            case Some(h) =>
              try {
                if (f(h)) go(i+1,j,t(),onEnd)
                else Left(h.toString) }
              catch { case e: Exception => Left(buildMsg(h, e)) }
            case None => Right((Unfalsified,i))
          }
          case _ => onEnd(i)
        }
      val endResult : Int => Result =
        if (n < max) i => Right((Proven,i))
        else i => Right((Exhausted, i))
      go(0, Math.min(n, max)/3, gen.exhaustive, endResult) match {
        case Right((Unfalsified,_)) =>
          val rands = randomStream(gen)(rng).map(Some(_))
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s // If proven or failed, stop immediately
      }
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = n / max + 1
      val props : List[Prop] =
        Stream.from(0).take(max + 1).map(i => forAll(g(i))(f)).toList
      props.map(p => Prop((max, n, rng) => p.run(max, casesPerSize, rng)))
           .reduceLeft(_ && _).run(max, n, rng)
    }
  }

  def forAll[A](g : Sized[A])(f : A => Boolean) : Prop =
    forAll(g.forSize)(f)

  def buildMsg[A](s : A, e : Exception) : String =
    "test case " + s + "\n" +
    "generated an exception" + e.getMessage + "\n" +
    "stack trace:\n" + e.getStackTrace.mkString("\n")


  def buildMsg[A](s : A) : String =
    "test case " + s + "\n" +
    "failed to pass the prop"

}


case class Gen[+A](sample: State[RNG, A], exhaustive : Stream[Option[A]]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(o => o.map(f)))
  def map2[B, C](g : Gen[B])(f : (A, B) => C) : Gen[C] = Gen(
    this.sample.map2(g.sample)(f),
    this.exhaustive.zipWith(g.exhaustive)((oa, ob) =>
      for {
        a <- oa
        b <- ob
      } yield f(a,b)
    )
  )
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    sample.flatMap(a => f(a).sample),
    Stream.empty
    /*exhaustive.flatMap {
      case None => Stream(None)
      case Some(a) => f(a).exhaustive
     }*/
  )

  def unsized : SGen[A] = Unsized(this)
}

object Gen {

  def choose(start: Int, stopExclusive : Int) : Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)),
        Stream.unfold(start)(x => if (x < stopExclusive) Some((Some(x), x+1)) else None)
    )

  def choose(i : Double, j : Double) : Gen[Double] =
    Gen(State(RNG.double).map(d => i + d % (j-i)),
        Stream.empty
    )

  def chooseViaMap(i : Double, j : Double) : Gen[Double] =
    uniform.map(d => i + d % (j-i))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream(Some(a)))

  def boolean : Gen[Boolean] = Gen(State(RNG.map(RNG.int)(x => x % 2 == 0)), Stream(Some(true), Some(false)))

  def uniform : Gen[Double] = Gen(State(RNG.double), Stream.empty)

  def listOfN[A](n : Int, g : Gen[A]) : Gen[List[A]] =
    g.flatMap((a : A) => if (n <= 0) Gen.unit(List()) else listOfN(n-1, g).map((l : List[A]) => a :: l))

  def sameParity(from : Int, to : Int) : Gen[(Int, Int)] =
    choose(from, to).map2(choose(from, to))((_, _)).map(t =>
      t match {
        case (d, e) if (d % 2 == 0 && e % 2 == 1) => (d, e-1)
        case (d, e) if (d % 2 == 1 && e % 2 == 0) => (d, e-1)
        case _ => t
      })

  def union[A](g1 : Gen[A], g2 : Gen[A]) : Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1 : (Gen[A], Double), g2 : (Gen[A], Double)) : Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    uniform.flatMap(d => if (d < g1Threshold ) g1._1 else g2._1)
  }

  def randomStream[A](gen : Gen[A])(rng : RNG) : Stream[A] =
    Stream.unfold(rng)(rng => Some(gen.sample.run(rng)))
}

trait SGen[+A]
case class Sized[+A](forSize : Size => Gen[A]) extends SGen[A]
case class Unsized[+A](get : Gen[A]) extends SGen[A]

object SGen {
  def listOfN[A](g : Gen[A]) : SGen[List[A]] =
    Sized(size => Gen.listOfN(size, g))
}
