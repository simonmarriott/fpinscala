package fpinscala.exercises.parsing

import fpinscala.exercises.testing.{Gen, Prop}

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]:
  self => // so inner classes may call methods of trait
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]
  def regex(r: Regex): Parser[String]
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _ ) | succeed(Nil)
    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def slice: Parser[String]
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      p.flatMap(a => p2.map(b => (a, b)))
    def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)

    def map2[B, C](p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      p.product(p2).map((a, b) => f(a, b))
    def flatMap[B](f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A])

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => succeed(a).run(s) == Right(a))

end Parsers

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[ParseError, Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
