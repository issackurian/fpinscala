/**
  * Created by ikurian on 1/16/16.
  */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }



  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  private def format(x: Int, fName: String, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(fName, x, f(x))
  }

  def main(args: Array[String]): Unit = {
    println(format(-42, "abs", abs))
    println(format(5, "factorial", factorial))
  }
}
