// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go (n, 1)
  }

  // Note that there's no return type declared. Scala is able to infer the
  // return type, but that's not a good practice. It's left out here because
  // it's a private method.
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"	// val -> immutable variable
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("factorial", 6, factorial))
  }
}
