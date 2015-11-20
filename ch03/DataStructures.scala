sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // Excercise 3.1, Page 34
    val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y  // Match
        case Cons(h, t) => h + sum(t) // Match but won't be hit
        case _ => 101 // Also a match but won't be hit
    }

    // Excercise 3.2, Page 35
    def tail[A](l: List[A]): List[A] =
        l match {
            case Nil => sys.error("tail of empty list")
            case Cons(_, t) => t
        }

    // Excercise 3.3, Page 36
    def setHead[A](h: A, l: List[A]): List[A] =
        l match {
            case Nil => sys.error("setHead of empty list")
            case Cons(_, t) => Cons(h, t)
        }
}

object DataStructures {
    def main(args: Array[String]): Unit = {
        println(List.x)
        println(List.tail(List(1,2,3,4,5)))
        println(List.setHead(6, List(1,2,3,4,5)))
    }
}
