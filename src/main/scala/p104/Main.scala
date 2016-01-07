package p104

import common.fibonacci.MatrixFibonacci
import common.numtheory.ModInt

/*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
It turns out that F541, which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). And F2749, which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.

Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.
 */
object Main extends App {
  val DIGITS = Set('1', '2', '3', '4', '5', '6', '7', '8', '9')

  def isAcceptable(l: BigInt): Boolean = {
    val str = l.toString
    isFrontAcceptable(str) && isBackAcceptable(str)
  }

  def isFrontAcceptable(str: String): Boolean = {
    str.take(9).toSet == DIGITS
  }

  def isBackAcceptable(str: String): Boolean = {
    str.takeRight(9).toSet == DIGITS
  }

  class truncIterator extends Iterator[Int] {
    var state = (ModInt(1, 1000000000), ModInt(0, 1000000000))

    override def hasNext: Boolean = true

    override def next(): Int = {
      val ret = state._2
      state = (state._1 + state._2, state._1)
      ret.value
    }
  }

  val start = System.currentTimeMillis()

  val truncIter = new truncIterator

  val fastFib = new MatrixFibonacci

  val value = truncIter.zipWithIndex.withFilter(p =>{
    val str = p._1.toString
    isBackAcceptable(str)
  }).map(_._2).find(n =>{
    println(s"n:$n")

    val Fn = fastFib(n)
    isFrontAcceptable(Fn.toString)
  })
  println(value)

  println(s"Completed in ${System.currentTimeMillis() - start} ms")
}
