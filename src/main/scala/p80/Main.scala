package p80

import common.TimedApp
import common.math.RootIterator
import common.numtheory.Prime

object Main extends TimedApp {
  /*
It is well known that if the square root of a natural number is not an integer, then it is irrational. The decimal expansion of such square roots is infinite without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.
  */

  val irrationals = (1 to 100).filterNot(Prime.isSquare(_))

  val sum = irrationals.map(i =>{
    val toSkip = math.sqrt(i).toInt.toString.length
    val iter = new RootIterator(i)
    iter.take(toSkip)
    iter.take(100).sum
  }).sum
  println(sum)
}
