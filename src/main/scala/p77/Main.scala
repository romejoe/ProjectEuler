package p77

import common.TimedApp
import common.numtheory.Prime
import common.util.Memonize

object Main extends TimedApp {
  /*
It is possible to write ten as the sum of primes in exactly five different ways:

7 + 3
5 + 5
5 + 3 + 2
3 + 3 + 2 + 2
2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?
  */

  val getPrimes = Memonize((l:Long) => Prime.lessThanEqual(l))

  val F:(Long,Long) => Set[List[Long]] = Memonize((i:Long, max:Long) => {
    val primes = getPrimes(math.min(i,max))

    val ret:Set[List[Long]] = primes.flatMap(s => {
      if (i - s == 0)
        Set(List(s))
      else {
        val subs = F(i - s, s)
        subs.map(s :: _)
      }
    }).toSet

    ret
  })

  val f = (N:Long) => F(N,N)
  val i = (2 to Int.MaxValue).find(i => f(i).size > 5000)
  println(s"i => $i")

}
