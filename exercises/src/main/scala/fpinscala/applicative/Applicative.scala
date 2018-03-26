package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List()) : F[List[A]])((fa, r) =>
      map2(fa, r)((a, l) => l :+ a)
    )

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    map(fa)(a => List.fill(n)(a))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a, b) => (a, b))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] ={
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a : => A) : (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def map2[A, B, C](fa : (F[A], G[A]), ga : (F[B], G[B]))(f : (A, B) => C) : (F[C], G[C]) =
        (self.map2(fa._1, ga._1)(f), G.map2(fa._2, ga._2)(f))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] ={
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a : => A) : F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga : F[G[A]], fgb : F[G[B]])(f : (A, B) => C) : F[G[C]] =
        self.map2[G[A], G[B], G[C]](fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map[K,V]())){
      case (f, (k, fv)) =>
        map2(f, fv)((m, v) => m + ((k, v)))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def flatMap[A, B](ma: Either[E, A])(f : A => Either[E, B]) : Either[E, B] =
        ma match {
          case Right(a) => f(a)
          case Left(e) => Left(e)
        }
      def unit[A](a : => A) : Either[E, A] = Right(a)
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      def unit[A](a : => A) : Validation[E, A] = Success(a)
      override def map2[A, B, C](a : Validation[E, A], b : Validation[E, B])(f: (A, B) => C) : Validation[E, C] =
        (a, b) match {
          case (Success(x),  Success(y)) => Success(f(x, y))
          case (Failure(e, le), Failure(e1, le1)) => Failure(e , (le.+:(e1) ++ le1))
          case (Failure(e, le), _)  => Failure(e, le)
          case (_ , Failure(e, le)) => Failure(e, le)
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))( (a, b) =>
        G.map2(f(a), b)((x : B, l : List[B]) => l :+ x)
      )
    override def sequence[G[_],A](fma: List[G[A]])(implicit G : Applicative[G]): G[List[A]] =
      fma.foldRight(G.unit(List[A]()))((ga, b) =>
        G.map2(ga, b)((a, l) => l :+ a)
      )
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]) : G[Option[B]] =
      sequence(fa.map(a => f(a)))
    override def sequence[G[_], A](fma : Option[G[A]])(implicit G : Applicative[G]) : G[Option[A]] =
      fma.foldRight(G.unit(None): G[Option[A]])((ga, goa) =>
        G.map2(ga, goa)((a, oa) => (Some(a) : Option[A]))
      )
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]) : G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)((ta) => traverse(ta)(f)))((b, lb) => Tree(b, lb))
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
