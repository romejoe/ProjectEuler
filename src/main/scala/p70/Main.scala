package p70

import common.TimedApp
import common.math.Rational
import common.numtheory.Prime

object Main extends TimedApp {
  /*
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

  */

  implicit class RicherString(str:String){
    def getCharacterFrequency() = {
      str.foldLeft(Map[Char,Int]().withDefaultValue(0))((m,c) => m.updated(c, m(c) + 1))
    }
    def isPermutation(s:String):Boolean = {
      if(str.length != s.length)
        false
      else{
        str.sorted == s.sorted
      }
    }
  }

  val primes = new Prime(10000000)
  val Ds = 2 to 10000000

  val m = Ds.map(d => (d, primes.EulerTotient(d)))
    .filter(p => p._1.toString.isPermutation(p._2.toString))
    .map(p => Rational(p._1,p._2)).min
  println(s"Max => ${m.n}")

}
