sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], value: A): List[A] = l match {
    case Nil => Cons(value, Nil)
    case Cons(_, xs) => Cons(value, xs)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    def go(l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_,xs) => if (n == 0) l else go(xs, n - 1)
    }
    go(l, n)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) dropWhile(xs,f) else l
  }

}
