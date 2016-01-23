package p67

import common.TimedApp


object Main extends TimedApp {
  /*
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)
 */

  val stream = getClass.getResourceAsStream("/p67/p67_triangle.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines
  val rows = lines.map(_.split(" ").map(_.toInt)).toSeq
  val base = rows.head.toIndexedSeq
  val res = rows.tail.foldLeft(base)((b,r) => {
    (r, b :+ 0, 0 +: b).zipped.map({case(i, a, b) => i + math.max(a,b)}).toIndexedSeq
  })
  println(s"Max => ${res.max}")


}
