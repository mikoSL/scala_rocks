object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
   (for {
      x <- 1 until limit
      newSet <- factors.filter(x % _ == 0)
     } yield x).sum
  }
}


// a better solution
object SumOfMultiples {
 def sum(factors: Set[Int], limit: Int): Int = {
  num.flatMap(num => num until limit by num).sum
 }
}
