package intro

/**
 * This part has some exercises for you to practice with the recursive lists and functions.
 * For the exercises in this part you are _not_ allowed to use library functions,
 * you should implement everything yourself.
 * Use recursion to process lists, iteration is not allowed.
 *
 * This part is worth 5 points.
 */
object Practice {

  /** Q5 (2p)
   * Implement the function that returns the first n elements from the list.
   * Note that `n` is an upper bound, the list might not have `n` elements.
   *
   * @param xs list to take items from.
   * @param n  amount of items to take.
   * @return the first n items of xs.
   */
  def firstN(xs: List[Int], n: Int, p: Int = 0): List[Int] = xs match {
    case Nil => List()
    case x :: tail if p < xs.size => {
      List.concat(List(x), firstN(tail, n, p + 1))
    }
    case x :: tail if p >= xs.size => {
      firstN(tail, n, p + 1)
    }
  }


  /** Q6 (3p)
   * Implement the function that returns the maximum value in the list.
   *
   * @param xs list to process.
   * @return the maximum value in the list.
   */
  def maxValue(xs: List[Int], currMax: Int = Integer.MIN_VALUE): Int = xs match {
    case Nil => currMax
    case x :: tail if x > currMax => {
      maxValue(tail, x)
    }
    case x :: tail if x <= currMax => {
      maxValue(tail, currMax)
    }
  }
}
