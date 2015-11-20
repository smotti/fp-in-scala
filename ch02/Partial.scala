object Partial {
  // The return type is kinda strange "B => C" (B to C). That means B must be
  // a function that takes one argument and produces C.
  private def partial1[A,B,C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def main(args: Array[String]): Unit = {
    val partial = partial1(4, (a: Int, b: Int) => a + b)
    println(partial(2))
  }
}
