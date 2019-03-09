// Implement isSorted, which checks whether and Array[A] is sorted according
// to a given comparison function.

object Exercise_2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.isEmpty) true
    else if (as.length == 1) true
    else ordered(as(0),as(1)) && isSorted(as.drop(2), ordered)
  }
}
