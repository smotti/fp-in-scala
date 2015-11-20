object Currying {

  // Convert a function f of two arguments into a function of one argument
  // that partially applies f.
  private def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // f(a) returns another function to which b is passed, and this returns another
  // function that takes a and b.
  private def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    val f = curry((a: Int, b: Int) => a + b)
    val g = f(1)
    println(g(1))

    // Why do we have to uncurry f instead of g?
    // Because f is of type A => B => C see the curry method and g is only B => C
    val h: (Int, Int) => Int = uncurry(f)
    println(h(1, 1))
  }
}
