package fpinscala.exercises.monoids

trait Foldable[F[_]]:

  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldMap(f.curried)(using dual(endoMonoid[B]))(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldMap(a => b => f(b, a))(using endoMonoid[B])(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldRight(mb.empty)((a, b) => mb.combine(f(a), b))

    def combineAll(using ma: Monoid[A]): A =
      as.foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      as.foldRight(List.empty[A])(_ :: _)

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMap(as.toList, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case Leaf(a) => f(a, acc)
        case Branch(l, r) => l.foldRight(r.foldRight(acc)(f))(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case Leaf(a) => f(acc, a)
        case Branch(l, r) => r.foldLeft(l.foldLeft(acc)(f))(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Leaf(a) => f(a)
        case Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case None => acc
        case Some(a) => f(a, acc)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case None => acc
        case Some(a) => f(acc, a)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case None => mb.empty
        case Some(a) => f(a)
