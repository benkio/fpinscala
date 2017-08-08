package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def string(s : String) : Parser[String]
  implicit def operators[A](p : Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a : A)(implicit f : A => Parser[String]) = ParserOps(f(a))

  def char(c : Char) : Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def or[A](s1 : Parser[A], s2 : => Parser[A]) : Parser[A]

  def run[A](p : Parser[A])(input : String) : Either[ParserError, A]

  def listOfN[A](n : Int, p : Parser[A]) : Parser[List[A]] =
    if (n <= 0) succeed(List()) else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p : Parser[A]) : Parser[List[A]] =
    or( map2(p, many(p))((a, l) => a :: l ),
        succeed(List()))

  def many1[A](p : Parser[A]) : Parser[List[A]] =
    map2(p, many(p))((a, la) => a :: la)

  def map[A, B](a : Parser[A])(f : A => B) : Parser[B]

  def succeed[A](a : A) : Parser[A] =
    string("").map(_ => a)

  def slice[A](p : Parser[A]) : Parser[String]

  def product[A, B](p1 : Parser[A], p2 : => Parser[B]) : Parser[(A, B)]

  def map2[A, B, C](p1 : Parser[A], p2 : => Parser[B])(f : (A, B) => C) : Parser[C] =
    map(product(p1, p2))(f.tupled)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2 : Parser[B]) : Parser[B] = self.or(p, p2)
    def or[B>:A](p2 : Parser[B]) : Parser[B] = self.or(p, p2)
    def **[B>:A](p2 : Parser[B]) : Parser[(A, B)] = self.product(p, p2)
    def product[B>:A](p2 : Parser[B]) : Parser[(A, B)] = self.product(p, p2)
    def many : Parser[List[A]] = self.many(p)
    def many1 : Parser[List[A]] = self.many1(p)
    def map[B](f : A => B) : Parser[B] = self.map(p)(f)
    def slice[A] : Parser[String] = self.slice(p)
  }

  object Laws {
    def equal[A](p1 : Parser[A], p2 : Parser[A])(in : Gen[String]) : Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p : Parser[A])(in : Gen[String]) : Prop =
      equal(p, p.map[A](a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParserError =
    ParserError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParserError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParserError] = List()) {
}
