package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) def distribute: (F[A], F[B]) =
    (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]]) def codistribute: F[Either[A, B]] =
    e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    
    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
//    as.foldLeft(unit(List[B]()))((acc, a) => f(a).flatMap(b => acc.flatMap(bs => unit(bs :+ b))))
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    fa.map(a => List.fill(n)(a))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(b => g(b))

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      compose(_ => fa, f)(())

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))
      ((a, acc) => f(a).map2(acc)((bool, filtered) => if bool then a :: filtered else filtered ))

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(fa => fa)

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      compose(a => fa.map(f), (fb: F[B]) => fb)(())

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).map(g).join

end Monad      

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A): Par[A] = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMapViaJoin(fa)(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A): P[A] = p.defaultSucceed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] =
        fa.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A): LazyList[A] = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]): LazyList[B] =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: => A): List[A] = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]): List[B] =
        fa.flatMap(f)

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = Id(a)
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]): Id[B] =
        fa.flatMap(f)

opaque type Reader[-R, +A] = R => A

object Reader:
  def ask[R]: Reader[R, R] = r => r
  def apply[R,A](f: R => A): Reader[R, A] = f

  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = _ => a
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]): R => B =
        r => f(fa(r)).run(r)
