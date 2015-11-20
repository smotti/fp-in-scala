object Compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    val composed = compose((x: Int) => x + 1, (x: Int) => x)
    println(composed(1))
  }
}
