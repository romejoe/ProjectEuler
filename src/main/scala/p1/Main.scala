package p1

object Main extends App{
  /**
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  */

  def sumIt(multiples:Seq[Int], end:Int):Int = {
    val items = multiples.flatMap(0.until(end, _)).toSet
    items.sum
  }

  println(sumIt(Seq(3,5), 10))
  println(sumIt(Seq(3,5), 1000))

}
