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
  def sumListIntProp(gen : Gen[List[Int]]) : Prop =
    forAll(gen){ l => l.sum == l.reverse.sum && l.sum == (0 :: l).sum }

  def maxListIntProp(gen: Gen[List[Int]]) : Prop =
    forAll(gen) { l => l.sortWith(_ > _).headOption == Try(l.max).toOption   }

}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

