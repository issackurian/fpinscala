package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def sumTailRec(ints: List[Int]): Int = {
    @annotation.tailrec
    def loop(xs: List[Int], acc: Int): Int = xs match {
      case Nil => acc
      case Cons(y,ys) => loop(ys, acc + y)
    }
    loop(ints, 0)
  }

  def fill[A](n: Int, a: A): List[A] = {
    @annotation.tailrec
    def loop(size: Int, x: A, acc: List[A]): List[A] = {
      if (size == 0) acc
      else loop(size-1, a, Cons(a,acc))
    }
    loop(n, a, Nil)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def productTailRec(ds: List[Double]): Double = {
    @annotation.tailrec
    def loop(xs: List[Double], acc: Double): Double = xs match {
      case Nil => acc
      case Cons(y, ys) => loop(ys, y * acc)
    }
    loop(ds, 1.0)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A],xs:List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(y, ys) => loop(Cons(y, acc), ys)
    }
    loop(Nil, l)
  }

  def applyTailRec[A](as: A*): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], xs: A*): List[A] = {
      if (xs.isEmpty) acc
      else loop(Cons(xs.head, acc), xs.tail: _*)
    }
    reverse(loop(Nil, as: _*))
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Exception
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception
      case Cons(y,Nil) => l
      case Cons(y1, Cons(y2, Nil)) => Cons(y1, Nil)
      case Cons(y, ys) => Cons(y, init(ys))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x: A, y: Int) => y + 1)

  def lengthTailRec[A](l: List[A]): Int = {
    @annotation.tailrec
    def loop(size: Int, xs: List[A]): Int = xs match {
      case Nil => size
      case Cons(y, ys) => loop(size + 1, ys)
    }
    loop(0, l)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
  }

  def foldLeftFR[A,B](l:List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l),z)((h, acc) => f(acc, h))
  }

  def foldLeftFR2[A,B](l:List[A], initial: B)(combiner: (B, A) => B): B = {
    type BtoB = B => B
    def delayer: BtoB = (b: B) => b

    foldRight(l: List[A], delayer: BtoB)((a: A, g: BtoB) => (b => g(combiner(b, a))))(initial)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightFL[A,B](l:List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l),z)((acc, h) => f(h, acc))
  }

  def foldRightFL2[A,B](l:List[A], z: B)(f: (A, B) => B): B = {
    type BtoB = B => B
    def delayer: BtoB = (b: B) => b

    foldLeft(l: List[A], delayer: BtoB)((g: BtoB, a: A) => (b => g(f(a, b))))(z)
  }

  def reverseFL[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))

  def sumFL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Int]): Int =
    foldLeft(l, 1)(_ * _)

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def mapFR[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h: A, acc: List[B]) => Cons(f(h), acc))

  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRightFL2(a1, a2)((h, acc) => Cons(h, acc))

  def flatten[A](l: List[List[A]]): List[A] = {
    foldLeftFR2(l, Nil: List[A])((acc: List[A], h: List[A]) => append(h, acc))
  }
  def flatten2[A](l: List[List[A]]): List[A] = {
    foldRightFL2(l, Nil: List[A])((h: List[A], acc: List[A]) => append(h, acc))
  }

  def addOne(l: List[Int]): List[Int] = mapFR(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = mapFR(l)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, acc) => {
      if (f(h)) Cons(h, acc)
      else acc
    })

  def flatMap[A](l: List[A])(f: A => List[A]): List[A] =
    foldRight(l, Nil: List[A])((h: A, acc: List[A]) => {
      val x = f(h)
      appendFR(x, acc)
    })

  def filterFM[A](l: List[A])(f: A => Boolean): List[A] ={
    def g(x: A) = if (f(x)) List(x) else Nil
    flatMap(l)(g)
  }

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = {
    def loop(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = xs match {
      case Nil => acc
      case Cons(hx, tailx) => ys match {
        case Nil => acc
        case Cons(hy, taily) => loop(tailx, taily, Cons(hx + hy, acc))
      }
    }
    reverse(loop(l1, l2, Nil: List[Int]))
  }

  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    def loop(xs: List[A], ys: List[A], acc: List[B]): List[B] = xs match {
      case Nil => acc
      case Cons(hx, tailx) => ys match {
        case Nil => acc
        case Cons(hy, taily) => loop(tailx, taily, Cons(f(hx,hy), acc))
      }
    }
    reverse(loop(l1, l2, Nil: List[B]))
  }

}
