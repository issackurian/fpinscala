package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def orElse2[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(x) => this
  }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) Some(a) else None)

  def filter2(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if (f(x)) this else None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    def naiveVariance(ys: Seq[Double], naiveMean: Double): Option[Double] = {
      val naiveDevs: Seq[Double] = ys.map(y => math.pow(y - naiveMean,2))
      mean(naiveDevs)
    }
    mean(xs).flatMap(m => naiveVariance(xs, m))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa =>
      b map(bb =>
        f(aa, bb)))

  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::xs => {
      val y: Option[List[A]] = sequence(xs)
      x flatMap((xx: A) => y.map((yy: List[A])=> xx::yy))
    }
  }

  def sequence2[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(Nil): Option[List[A]])((x: Option[A], y: Option[List[A]]) => x flatMap((xx: A) => y.map((yy: List[A]) => xx::yy)))

  def sequence3[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(Some(Nil): Option[List[A]])((x, y) => map2(x, y)(_::_))

  def sequence4[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)((x: Option[A]) => x)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a: A, b: Option[List[B]]) => (f(a).flatMap(aa => b.map(bb => aa::bb))))

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map(i => f(i)))
}