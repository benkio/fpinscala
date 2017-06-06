package fpinscala.testing

import fpinscala.laziness.Stream
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

trait Prop {
/*  def sumListIntProp(gen : Gen[List[Int]]) : Prop =
    forAll(gen){ l => l.sum == l.reverse.sum && l.sum == (0 :: l).sum }

  def maxListIntProp(gen: Gen[List[Int]]) : Prop =
    forAll(gen) { l => l.sortWith(_ > _).headOption == Try(l.max).toOption   }
 */
  def check : Either[FailedCase, SuccessCount]
  def &&(p : Prop) : Prop = new Prop {
    override def check = (this.check, p.check) match {
      case (Left(s), Right(_)) => Left(s)
      case (Right(_), Left(s)) => Left(s)
      case (Left(s), Left(r))  => Left(s ++ "\n" ++r)
      case (Right(i), Right(j))=> Right(i + j)
    }
  }
}

object Prop {
  type FailedCase   = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


case class Gen[+A](sample: State[RNG, A], exaustive : Stream[Option[A]]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exaustive.map(o => o.map(f)))
  def map2[B, C](g : Gen[B])(f : (A, B) => C) : Gen[C] = Gen(
    this.sample.map2(g.sample)(f),
    this.exaustive.zipWith(g.exaustive)((oa, ob) =>
      for {
        a <- oa
        b <- ob
      } yield f(a,b)
    )
  )
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    sample.flatMap(a => f(a).sample),
    Stream.empty
    /*exaustive.flatMap {
      case None => Stream(None)
      case Some(a) => f(a).exaustive
     }*/
  )


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

}



trait SGen[+A] {

}

