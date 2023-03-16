package Utils

/** Contains the function necessary to calculate the number of *clinks* when n
  * people want to cheers.
  */
object ClinksCalculator:
  /** Calculate the factorial of a given number
    * @param n
    *   the number to compute
    * @return
    *   n!
    */
  // TODO - Part 1 Step 1
  def factorial(n: Int): BigInt =
    def loop(acc: BigInt, n: Int): BigInt =
      if n == 0 then acc
      else loop(acc * n, n - 1)
    loop(1, n)
  end factorial

  /** Calculate the combination of two given numbers
    * @param n
    *   the first number
    * @param k
    *   the second number
    * @return
    *   n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int =
    val comb = factorial(n) / (factorial(k) * factorial(n - k))
    comb.toInt
  end calculateCombination

end ClinksCalculator
