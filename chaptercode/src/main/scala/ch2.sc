def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n-1, n*acc)

  go(n, 1)
}

factorial(4)
factorial(5)
factorial(6)

def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int, curr: Int): Int = {
    if (n == 0) prev
    else go(n-1, curr, prev + curr)
  }
  go(n, 0, 1)
}

fib(0)
fib(1)
fib(2)
fib(3)

//ordered(as(n), as(n+1))
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = {
    if (n >= as.length-1) true
    else if (!ordered(as(n),as(n+1))) false
    else loop(n+1)
  }
  loop(0)
}

isSorted(Array(0,1,2,3,4), (x: Int, y: Int) => x < y )
isSorted(Array(4,3,2,1), (x: Int, y: Int) => x < y)

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

def curry2[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => f(a,_)

val f = (x: Int, y: Int) => x + y

val g = curry(f)

g(10)(9)



def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)


def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) =>f(g(a))

def compose2[A,B,C](f: B => C, g: A => B): A => C =
  g andThen f

def compose3[A,B,C](f: B => C, g: A => B): A => C =
  f compose g