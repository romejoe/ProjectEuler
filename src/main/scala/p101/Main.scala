package p101

import breeze.linalg._
import common.util.Memonize

import common.TimedApp

object Main extends TimedApp{
  /*
  If we are presented with the first k terms of a sequence it is impossible to say with certainty the value of the next term, as there are infinitely many polynomial functions that can model the sequence.

As an example, let us consider the sequence of cube numbers. This is defined by the generating function,
un = n3: 1, 8, 27, 64, 125, 216, ...

Suppose we were only given the first two terms of this sequence. Working on the principle that "simple is best" we should assume a linear relationship and predict the next term to be 15 (common difference 7). Even if we were presented with the first three terms, by the same principle of simplicity, a quadratic relationship should be assumed.

We shall define OP(k, n) to be the nth term of the optimum polynomial generating function for the first k terms of a sequence. It should be clear that OP(k, n) will accurately generate the terms of the sequence for n ≤ k, and potentially the first incorrect term (FIT) will be OP(k, k+1); in which case we shall call it a bad OP (BOP).

As a basis, if we were only given the first term of sequence, it would be most sensible to assume constancy; that is, for n ≥ 2, OP(1, n) = u1.

Hence we obtain the following OPs for the cubic sequence:

OP(1, n) = 1	1, 1, 1, 1, ...
OP(2, n) = 7n−6	1, 8, 15, ...
OP(3, n) = 6n2−11n+6     	1, 8, 27, 58, ...
OP(4, n) = n3	1, 8, 27, 64, 125, ...
Clearly no BOPs exist for k ≥ 4.

By considering the sum of FITs generated by the BOPs (indicated in red above), we obtain 1 + 15 + 58 = 74.

Consider the following tenth degree polynomial generating function:

un = 1 − n + n2 − n3 + n4 − n5 + n6 − n7 + n8 − n9 + n10

Find the sum of FITs for the BOPs.
   */
  implicit class RichIntt(i:Int){
    def **(e:Int):Int = {
      var t = 0
      var ret:Int = 1
      while(t < e){
        ret *= i
        t += 1
      }
      ret
    }
  }
  implicit class RichDoublee(d:Double){
    def round(precision:Double = 10E-10) :Double = {
      val (floor, ceil) = (math.floor(d), math.ceil(d))
      if(math.abs(d-floor) < precision)
        floor
      else if(math.abs(ceil-d) < precision)
        ceil
      else
        d
    }
  }

  def u_n(n:Double):Double = {
    1 - n + math.pow(n,2) - math.pow(n, 3) + math.pow(n, 4) - math.pow(n, 5) + math.pow(n, 6) - math.pow(n, 7) + math.pow(n, 8) - math.pow(n, 9) + math.pow(n, 10)
  }

  val buildXMatrix = Memonize((n:Int) => {
    new DenseMatrix(n, n, n.to(1, -1).flatMap(i => (n - 1).to(0, -1).map(pow => (i ** pow).toDouble)).toArray).t
  })

  val buildXMatrix2 = Memonize((x:Double, n:Int) => {
    new DenseMatrix(n,1, (n-1).to(0,-1).map(p => math.pow(x,p)).toArray)
  })

  def buildEstimateFunction(seq:Array[Double]):(Double => Double) = {
    val n = seq.length
    val X = buildXMatrix(n)
    val A = X

    val y = new DenseMatrix(n,1,seq.reverse)

    val tmp = A \ y

    new ((Double) => Double) {
      override def apply(x: Double): Double = {
        val xs = buildXMatrix2(x,n)
        val ans =  tmp.t * xs
        ans(0,0)
      }
    }
  }

  val n = 10

  val master = (1 to n+1).map(u_n(_)).toArray

  val FITs = (1 to n).map(t => {
    val f = buildEstimateFunction(master.take(t))
    //val err = (1 to t).map(i => math.pow(f(i)-master(i-1),2.0)).sum
    f(t+1)
  })

  println(FITs.sum.round(10E-5).toLong)

}
