package p5

import common.numtheory.Factorize

object Main extends App{
  /**
  *2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

*What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  */


  val factors = (1 to 20).flatMap(Factorize.naiveFactor(_).toSet).groupBy(_._1).mapValues(_.map(_._2).max)
  val num = factors.foldLeft(1)({case (i, p) =>{
    (i*math.pow(p._1, p._2)).toInt
  } })

  println(num)
}
