package p4

object Main extends App{
  /**
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  */

  println((100 to 999).combinations(2).map(_.product).filter(i => i.toString == i.toString.reverse).max)

}
