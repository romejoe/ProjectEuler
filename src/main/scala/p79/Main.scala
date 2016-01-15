package p79

import common.TimedApp
import common.RicherInt
import common.math.matrix.VectorMatrix

object Main extends TimedApp {
  /*
A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.
  */

  val stream = getClass.getResourceAsStream("/p79/p79_keylog.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines
  //lines.map(_.toInt.toDigits).foreach(digits => println(digits.mkString("-")))
  val tmp:Iterator[(Int,Int)] = lines.flatMap(i =>{
    val digits = i.toInt.toDigits
    digits.combinations(2).map(seq => (seq(0), seq(1)))
  })
  val counts = tmp.foldLeft(Map[(Int,Int),Int]() withDefaultValue 0){(m,x) => m + (x -> (1 /*+ m(x)*/))}
  //val counts = tmp.foldLeft(Map[(Int,Int),Int]() withDefaultValue 0){(m,x) => m + (x -> 1)}
  counts.foreach(println)
  val m = counts.foldLeft(new VectorMatrix[Int](Vector.fill(10, 10)(0))){(m,p) =>
    m.updated(p._1._1, p._1._2, p._2)
  }

  println()
  println(m)
  println()
  val inDegrees = m.cols.toArray.zipWithIndex.map(p => (p._2, p._1.sum)).sortBy(_._2).toMap
  println()
  val outDegrees = m.rows.toArray.zipWithIndex.map(p => (p._2, p._1.sum)).sortBy(_._2).toMap
  val source = (0 until 10)
    //remove standalone nodes
    .filterNot(i => outDegrees(i) == 0 && inDegrees(i) == 0)
      .find(i => inDegrees(i) == 0).get

  println(source)
  println(inDegrees)
  println()
  println(outDegrees)
  println(outDegrees.toIndexedSeq.sortBy(-_._2))
}
