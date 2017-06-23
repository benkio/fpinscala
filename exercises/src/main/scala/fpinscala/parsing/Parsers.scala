package fpinscala.parsing

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c : Char) : Parser[Char]

  def orA(s1 : A, s2 : A) : Parser[A]

  def run[A](p : Parser[A])(input : String) : Either[ParserError, A]

  def listOfN[A](n : Int, p : Parser[A]) : Parser[List[A]]

  implicit def string(s : String) : Parser[String]
  implicit def operators[A](p : Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](implicit f : A => Parser[String]) = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2 : Parser[B]) : Parser[B] = self.orA(p, p2)
    def or[B>:A](p2 : Parser[B]) : Parser[B] = self.orA(p, p2)
  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
