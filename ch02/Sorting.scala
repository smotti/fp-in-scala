object Sorting {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i > 0) {
	if (ordered(as(i-1), as(i))) loop(i-1)
	else false
      }
      else true
    }

    loop(as.length - 1)
  }

  def ordered(x: Int, y: Int): Boolean = {
    if (x <= y) true
    else false
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Int] = Array(3, 2, 4, 1)
    if (isSorted(a, ordered)) println("Array is sorted")
    else println("Array isn't sorted")
  }
}
