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


  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs,z)(f))
    }

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  // 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11 - sum, product and length in terms of foldLeft
  def sum1(ints: List[Int]): Int = foldLeft(ints,0)(_ + _)

  def product1(l: List[Double]): Double = foldLeft(l,1.0)(_ * _)

  def length1[A](l: List[A]): Int = foldLeft(l,0)((acc,x) => acc + 1)

  // 3.12
  def reverse(l: List[Int]): List[Int] = {
    def go(l: List[Int], acc: List[Int]): List[Int] = l match {
      case Nil => acc
      case Cons(x,xs) => go(xs, Cons(x, acc))
    }
    go(l, Nil)
  }

  // TODO: Fix it
  // def reverse1(l: List[Int]): List[Int] = foldLeft(l,List[Nothing])((x, acc: List[Nothing]) => Cons(x,acc))

  // 3.13 - foldLeft in terms of foldRight

  // Implement foldLeft in terms of foldRight
  def foldLeft1[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((x, acc) => f(acc, x))
  }

  // Implement foldRight in terms of foldLeft
  def foldRight1[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def swap[A,B,C](f: (A,B) => C): B => (A => C) = (b: B) => (a: A) => f(a, b)
    foldLeft(as: List[A], z: B)((acc, x) => swap(f)(x)(acc))
  }
}

