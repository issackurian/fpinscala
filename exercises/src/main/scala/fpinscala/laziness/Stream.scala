package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, acc) => cons(f(h()), acc().map(f))
    case _ => Empty
  }

  def mapFR[B](f: A => B): Stream[B] =
  this.foldRight(empty[B])((h: A, acc) => cons(f(h), acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(empty[B])((h,acc) => f(h) append acc)

  def flatMap1[B](f: A => Stream[B]): Stream[B] = {
    this match {
      case Cons(h, t) => f(h()) append(t().flatMap1(f))
      case _ => empty[B]
    }
  }

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A]) {
      (h, acc) => if (f(h)) cons(h, acc)
                else acc
    }

  def append[B>:A](a2: => Stream[B]): Stream[B] =
    this.foldRight(a2){(h, acc) => cons(h, acc)}



  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //TODO tail rec
  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Cons(h, t) => Cons(h, () => t().take(n-1))
      case _ => this
    }
  }

  //TODO tail rec, what if n greater than size of list?
  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else this match {
      case Cons(h, t) => t().drop(n-1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p))
                      else Empty
    case _ => this
  }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A]){
      (x, y) => {
        if (p(x)) Cons(() => x, () => y)
        else Empty
      }
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p)
                      else false
    case _ => true
  }

  def forAllFR(p: A => Boolean): Boolean = {
    this.foldRight(true)((x, y) => p(x) && y)
  }

  def forAll2(p: A => Boolean): Boolean = this.exists(p(_) == false) == false

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def headOptionFR: Option[A] =
    this.foldRight(None: Option[A]){ (h, _) => Some(h) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")


  //TODO tail rec implementation
  def toList: List[A] = this match {
    case Cons(h, t) => h()::t().toList
    case _ => List()
  }

  def toListTR: List[A] = {
    @annotation.tailrec
    def loop(a: Stream[A], acc: List[A]): List[A] = a match {
      case Cons(h, t) => loop(t(), h()::acc)
      case _ => acc
    }
    loop(this, List()).reverse
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    def unfolder(x: Stream[A]): Option[(B, Stream[A])] = x match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

    unfold(this)(unfolder)
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    // state - (n: int, s: stream)
    def unfolder(st: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = st match {
      case (Cons(h, t), 0) => None
      case (Cons(h, t), x) => Some(h(), (t(), x-1))
      case _ => None
    }
    unfold((this, n))(unfolder)
  }

  def takeViaUnfoldFromKey(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }


  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    def unfolder(xs: Stream[A]): Option[(A, Stream[A])] = xs match {
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case Empty => None
    }
    unfold(this)(unfolder)
  }

  def zipWith[B, C](xs: Stream[C])(f: (A, C) => B): Stream[B] = {
    def unfolder(initState: (Stream[A], Stream[C])): Option[(B, (Stream[A], Stream[C]))] = initState match {
      case (Cons(ha, ta), Cons(hc, tc)) => Some((f(ha(), hc()), (ta(), tc())))
      case _ => None

    }

    unfold((this, xs))(unfolder)
  }

  def zipAll[B](xs: Stream[B]): Stream[(Option[A], Option[B])] = {
    def unfolder(initState: (Stream[A], Stream[B])):
                  Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = initState match {
      case (Cons(ha, ta), Cons(hc, tc)) => Some(((Some(ha()), Some(hc())), (ta(), tc())))
      case (Empty, Cons(hc, tc)) => Some(((None, Some(hc())), (Empty, tc())))
      case (Cons(ha, ta), Empty) => Some(((Some(ha()), None), (ta(), Empty)))
      case _ => None

    }

    unfold((this, xs))(unfolder)
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val ones2 = unfold(1)((x: Int) => Some(1, 1))

  def constant[A](a: A): Stream[A] = {
    lazy val constantStream: Stream[A] = cons(a, constantStream)
    constantStream
  }

  def constant2[A](a: A): Stream[A] = {
    unfold[A, A](a)((x) => Some(x,x))
  }

  def from(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = from(n + 1)
    cons(n, tail)
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)((x) => Some(x, x+1))
  }

  def fibs(): Stream[Int] = {
    def loop(first: Int, second: Int): Stream[Int] = {
      lazy val next = first + second
      cons(first, loop(second, next))
    }
    loop(0, 1)
  }

  def fibs2(): Stream[Int] = {
    lazy val rest = unfold[Int, (Int, Int)]((0, 1)){ (x: (Int, Int)) =>
      val value = x._1 + x._2
      val state = (x._2, value)
      Some(value, state)
    }
    cons(0, cons(1, rest))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val next: Option[(A, S)] = f(z)
    next match {
      case Some((value, state)) => cons(value, unfold(state)(f))
      case None => empty
    }
  }


}
