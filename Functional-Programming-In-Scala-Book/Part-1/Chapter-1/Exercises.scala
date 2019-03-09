
object Chapter_1 {

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if (n == 0) current
      else go(n - 1, next, current + next)
    go(n, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.isEmpty) true
    else if (as.length == 1) true
    else ordered(as(0),as(1)) && isSorted(as.drop(2), ordered)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a, b)

  // 2.5
  def compose[A,B,C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))
}
