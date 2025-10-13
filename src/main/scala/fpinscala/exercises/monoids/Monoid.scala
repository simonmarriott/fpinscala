package fpinscala.exercises.monoids

import fpinscala.exercises.monoids.Monoid.WC.{Part, Stub}
import fpinscala.exercises.parallelism.Nonblocking.Par

trait Monoid[A]:
  def combine(a1: A, a2: A): A

  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2

    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2

    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2

    def empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2

    def empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def empty: Option[Nothing] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)

    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f: A => A, g: A => A): A => A = f andThen g

    def empty: A => A = identity

  import fpinscala.exercises.testing.{Gen, Prop}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case a ** b ** c =>
          m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
      .tag("associativity")
    val identity = Prop
      .forAll(gen): a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      .tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def empty = Par.unit(m.empty)

    def combine(a: Par[A], b: Par[A]) = a.map2(b)(m.combine)

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap: bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))

  case class Interval(ordered: Boolean, min: Int, max: Int)

  val orderedMonoid: Monoid[Option[Interval]] = new:
    def combine(oa1: Option[Interval], oa2: Option[Interval]): Option[Interval] =
      (oa1, oa2) match
        case (Some(a1), Some(a2)) => Some(Interval(a1.ordered && a2.ordered && a1.max <= a2.min, a1.min, a2.max))
        case (x, None) => x
        case (None, x) => x

    val empty: Option[Interval] = None

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => Some(Interval(true, i, i))).forall(_.ordered)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  val wcMonoid: Monoid[WC] = new:
    val empty: WC = WC.Stub("")

    def combine(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b)) => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l, w, r), WC.Part(l2, w2, r2)) =>
        WC.Part(l, w + (if (r + l2).isEmpty then 0 else 1) + w2, r2)

  def wcGen: Gen[WC] =
    val smallString = Gen.choose(0, 10).flatMap(Gen.stringN)
    val genStub = smallString.map(s => WC.Stub(s))
    val genPart = for
      lStub <- smallString
      words <- Gen.choose(0, 10)
      rStub <- smallString
    yield WC.Part(lStub, words, rStub)
    Gen.union(genStub, genPart)

  val wcMonoidTest: Prop = monoidLaws(wcMonoid, wcGen)

  def count(s: String): Int =
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace then
        WC.Part("", 0, "")
      else
        WC.Stub(c.toString)

    def unstub(s: String) = if s.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(s) => unstub(s)
      case WC.Part(l, w, r) => unstub(l) + w + unstub(r)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)): (A, B) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))

    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B): A => B = a => mb.combine(f(a), g(a))

    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(k, mv.combine(a.getOrElse(k, mv.empty),
                                  b.getOrElse(k, mv.empty)))

    val empty: Map[K, V] = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    as.foldLeft(Map[A, Int]())((acc, a) =>
      acc.updated(a, intAddition.combine(1, acc.getOrElse(a, intAddition.empty))))
  }

end Monoid
