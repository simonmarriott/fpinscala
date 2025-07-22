package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.state.RNG.double
import fpinscala.exercises.testing.Prop.Result.{Falsified, Passed}

import scala.annotation.tailrec

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
import fpinscala.exercises.testing.Prop.*

//trait Prop
opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type MaxSize = Int

  opaque type TestCases = Int

  object TestCases:
    extension (x: TestCases) def toInt: Int = x

    def fromInt(x: Int): TestCases = x

  opaque type FailedCase = String

  object FailedCase:
    extension (f: FailedCase) def string: String = f

    def fromString(s: String): FailedCase = s

  opaque type SuccessCount = Int

  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x

    def fromInt(x: Int): SuccessCount = x

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true

  extension (self: Prop)
    def check(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Result =
      self(maxSize, testCases, rng)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, n, rng) => self.tag("and-left")(max, n, rng) match
        case Passed => that.tag("and-right")(max, n, rng)
        case x => x

    def ||(that: Prop): Prop =
      (max, n, rng) => self.tag("or-left")(max, n, rng) match
        // In case of failure, run the other prop.
        case Falsified(msg, _) => that.tag("or-right").tag(msg.string)(max, n, rng)
        case x => x

    /* This is rather simplistic - in the event of failure, we simply wrap
     * the failure message with the given message.
     */
    def tag(msg: String): Prop =
      (max, n, rng) => self(max, n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x
end Prop

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] = State[RNG, List[A]](s =>
      Range(0, n).foldLeft((List.empty[A], s)) { case ((as, rng), i) =>
        val (a, r) = self.next(rng)
        (as :+ a, r)
      }
    )

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      self.flatMap(a =>
        State[RNG, List[A]](s =>
          val (n, rng) = size.next(s)
          Range(0, n).foldLeft((List.empty[A], s)) { case ((as, rng), i) =>
            val (a, r) = self.next(rng)
            (as :+ a, r)
          }
        )
      )

    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)

    def map2[B,C](that: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(that)(f)

    def list: SGen[List[A]] =
      n => listOfN(n)

    def nonEmptyList: SGen[List[A]] =
      n => listOfN(n.max(1))

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

  def unit[A](a: => A): Gen[A] =
    State(s => (a, s))

  def boolean: Gen[Boolean] =
    State(s =>
      val (d, r) = double(s)
      (d > 0.5, r)
    )

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b.equals(true) then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    State(s =>
      val total = g1._2 + g2._2
      val (d, r) = double(s)
      if d < g1._2 / total then g1._1.next(r) else g2._1.next(r)
    )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    @tailrec
    def loop(rng: RNG, begin: Int, end: Int): (Int, RNG) =
      val (i, r) = rng.nextInt
      if i >= start && i < stopExclusive
      then (i, r) else loop(r, start, stopExclusive)

    State[RNG, Int](s =>
      loop(s, start, stopExclusive)
    )

  extension [A](self: Gen[A])
    def unsized: SGen[A] = _ => self

//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

//trait SGen[+A]
opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)

    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, self(size))

    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def list: SGen[List[A]] =
      n => listOfN(n)

    def nonEmptyList: SGen[List[A]] =
      n => listOfN(n.max(1))

    def unsized: SGen[A] = n => self(n)

    def map[B](f: A => B): SGen[B] =
      self(_).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(f(_)(n))

    def map2[B, C](that: SGen[B])(f: (A, B) => C): SGen[C] =
      n => self(n).map2(that(n))(f)