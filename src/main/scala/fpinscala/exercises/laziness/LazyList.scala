package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty

  def drop(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => Cons(() => h(), t)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if (p(h())) => Cons(() => h(), t)
    case _ => Empty

  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapViaMatch[B](f: A => B): LazyList[B] = this match
    case Empty => Empty
    case Cons(h, t) => Cons(() => f(h()), () => t().map(f))

  def map[B](f: A => B): LazyList[B] = foldRight(Empty: LazyList[B])(
    (a, b) => Cons(() => f(a), () => b)
  )

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold[B, LazyList[A]](this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold(this, n){ (l, n) => l match
      case Cons(h, t) if n > 0 => Some(h(), (t(), n -1))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this){
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C) : LazyList[C] =
    unfold((this, that)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h2, t2), Empty) => Some((Some(h2()), None), (t2(), Empty))
      case _ => None
    }

  def filter(f: A => Boolean): LazyList[A] = foldRight(Empty: LazyList[A])(
    (a, b) => if f(a) then Cons(() => a, () => b) else b
  )

  def append[B >: A](list: LazyList[B]): LazyList[B] =
    foldRight(list)((a, b) => Cons(() => a, () => b))

  def flatMap[B >: A](f: B => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, b) =>
      f(a).append(b)
    )

  def startsWith[B](s: LazyList[B]): Boolean = zipAll(s).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    .append(LazyList(Empty))
  
  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, Cons(() => b2, () => b1(1)))
    .apply(1)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(last: Int, next: Int): LazyList[Int] =
      cons(next, go(next, last + next))
    cons(0, go(0, 1))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None => empty
    case Some(a, s) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = unfold(0, 1)((last, next) => Some(last, (next, next + last)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(x => Some(x, x + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(n => Some(n, n))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(n => Some(n, n))
