// Write a recursive function to get the nth Fibonacci number.
// The first two Fibonacci numbers are 0 and 1.
// The nth number is always the sum of the previous two -
// the sequence begins 0, 1, 1, 2, 3, 5.

object Exercise_2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if (n == 0) current
      else go(n - 1, next, current + next)
    go(n, 0, 1)
  }
}
