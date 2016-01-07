package p3

import common.numtheory.Factorize._

object Main extends App{
  /**
    * The prime factors of 13195 are 5, 7, 13 and 29.

    * What is the largest prime factor of the number 600851475143 ?
  */
  //println(naiveFactor(13195))
  val factors = naiveFactor(600851475143L)
  println(factors.keySet.max)
}
