package p100

import common.math.Rational
import common.util.Collection.constSeq
import common.util.Memonize
import common.{RicherLong, _}

object Main extends TimedApp {
  /*
  If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box containing eighty-five blue discs and thirty-five red discs.

By finding the first arrangement to contain over 1012 = 1,000,000,000,000 discs in total, determine the number of blue discs that the box would contain.
   */

  //https://oeis.org/A079496
  val a: Long => Long = Memonize((n: Long) => {
    n match {
      case 0 | 1 => 1
      case _ =>
        if (n.isEven) {
          4 * a(n - 1) - a(n - 2)
        }
        else {
          2 * a(n - 1) - a(n - 2)
        }
    }
  })

  def generateContinuedFraction(n: Long): Seq[Long] = {
    Seq(0L, 1L) ++ (if ((n & 1) == 0) constSeq(n, 2L) :+ 3L else constSeq(n + 1, 2L))
  }

  def continuedFractionToRational(continuedFraction: Seq[Long]): Rational = {
    val rev = continuedFraction.reverse
    val base = rev.head
    rev.tail.foldLeft(Rational(base, 1))((s, a) => s.flip + a)
  }

  def f(n: Long): Rational = {
    continuedFractionToRational(generateContinuedFraction(n)) * Rational(a(n + 1), a(n + 1))
  }

  val result = (0L to 100L).view.map(n => (n, f(n))).find(_._2.d > 1000000000000L)
  if (result.isDefined) {
    println(s"Result => ${result.get}")
  }
  else {
    println("Not Found")
  }
}
