package fpinscala.datastructures

/**
  * trait => an abstract interface
  * sealed => all implementations of List must be declared in this file
  * + means covariant (i.e. for all types X and Y, if X is a subtype of Y,
  * then List[X] is a subtype of List[Y])
  */
sealed trait List[+A]

// List constructors
case object Nil extends List[Nothing]	// Empty List
case class Cons[+A](head: A, tail: List[A]) extends List[A]   // Nonempty list

// List companion object
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // Variadic function syntax
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
